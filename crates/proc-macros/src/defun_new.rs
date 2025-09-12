#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{Error, Type};

use crate::{
    defun::inventory_submit,
    function::{Arg, ArgKind, Function, RetKind},
};

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let lisp_name = spec.name.unwrap_or_else(|| subr_name.clone());
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let rust_wrapper_name = format_ident!("__rust_wrapper_{}", &subr_name);

    let args = function.args;
    let arg_conversion = get_arg_conversion(&args);

    // Generate the extern "C" function signature
    let mut c_params: Vec<TokenStream> = Vec::new();
    let mut c_param_idents: Vec<Ident> = Vec::new();

    for arg in args.iter() {
        let arg_info = &arg.info;
        let ident = &arg.ident;
        match &arg_info.kind {
            ArgKind::Env => {
                c_params.push(quote! { env: i64 });
                c_param_idents.push(format_ident!("env"));
            }
            ArgKind::Slice(_) => {
                let ptr_ident = format_ident!("{}_ptr", ident);
                let argc_ident = format_ident!("{}_argc", ident);
                c_params.push(quote! { #ptr_ident: i64 });
                c_params.push(quote! { #argc_ident: i64 });
                c_param_idents.push(ptr_ident);
                c_param_idents.push(argc_ident);
            }
            _ => {
                c_params.push(quote! { #ident: i64 });
                c_param_idents.push(ident.clone());
            }
        }
    }

    // Generate the actual function call
    let call_args = args.iter().map(|arg| {
        let ident = &arg.ident;
        quote! { #ident }
    });

    // let ret_is_unit = matches!(function.ret_kind, RetKind::Unit);
    let wrapper_ret_ty = quote! { ::std::result::Result<i64, &'static str> };

    let wrapper_result = match &function.ret_kind {
        RetKind::Unit => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(_) => Ok(crate::core::value::NIL as i64),
                        Err(e) => {
                            tracing::error!("error: {e:?}");
                            Err("call error")
                        }
                    }
                }
            } else {
                quote! {
                    Ok(crate::core::value::NIL as i64)
                }
            }
        }
        RetKind::Value => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(val) => Ok(val.0 as i64),
                        Err(e) => {
                            tracing::error!("error: {e:?}");
                            Err("call error")
                        }
                    }
                }
            } else {
                quote! { Ok(result.0 as i64) }
            }
        }
        RetKind::IntoValue => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(val) => {
                            let v: crate::core::value::Value = val.tag();
                            Ok(v.0 as i64)
                        }
                        Err(e) => {
                            tracing::error!("error: {e:?}");
                            Err("call error")
                        }
                    }
                }
            } else {
                quote! {
                    let v: crate::core::value::Value = result.tag();
                    Ok(v.0 as i64)
                }
            }
        }
        RetKind::Primitive(_ident) => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(val) => Ok(val as i64),
                        Err(e) => {
                            tracing::error!("error: {e:?}");
                            Err("call error")
                        }
                    }
                }
            } else {
                quote! { Ok(result as i64) }
            }
        }
        RetKind::Option(inner) => {
            let some_to_value = match inner.as_ref() {
                RetKind::Value => quote! { v },
                RetKind::IntoValue => quote! {{
                    let vv: crate::core::value::Value = ::std::convert::Into::into(v);
                    vv
                }},
                _ => quote! {{
                    let vv: crate::core::value::Value = ::std::convert::Into::into(v);
                    vv
                }},
            };
            if function.fallible {
                quote! {
                    match result {
                        Ok(opt) => {
                            let ret = match opt {
                                Some(v) => {
                                    let __val = { #some_to_value };
                                    __val.0 as i64
                                }
                                None => crate::core::value::NIL as i64,
                            };
                            Ok(ret)
                        }
                        Err(e) => {
                            tracing::error!("error: {e:?}");
                            Err("call error")
                        }
                    }
                }
            } else {
                quote! {
                    let ret = match result {
                        Some(v) => {
                            let __val = { #some_to_value };
                            __val.0 as i64
                        }
                        None => crate::core::value::NIL as i64,
                    };
                    Ok(ret)
                }
            }
        }
        _ => {
            let val_fn = |val| {
                quote! {
                    unsafe {
                        // val as *const u8 as i64
                        std::mem::transmute::<_, i64>(#val)
                    }
                }
            };
            let val = val_fn(format_ident!("val"));
            let res = val_fn(format_ident!("res"));
            if function.fallible {
                quote! {
                    match result {
                        Ok(val) => Ok(#val),
                        Err(e) => {
                            tracing::error!("error: {e:?}");
                            Err("call error")
                        }
                    }
                }
            } else {
                quote! { Ok(#res) }
            }
        }
    };

    let extern_fn = quote! {
        #[automatically_derived]
        #[doc(hidden)]
        unsafe extern "C" fn #func_name(#(#c_params),*) -> i64 {
            match #rust_wrapper_name(#(#c_param_idents),*) {
                Ok(v) => v,
                Err(e) => {
                    tracing::error!("error: {e:?}");
                    crate::core::value::NIL as i64
                }
            }
        }
    };

    let mut signatures: Vec<TokenStream> = Vec::new();
    for arg in args.iter() {
        match &arg.info.kind {
            ArgKind::Env => {
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // env
                });
            }
            ArgKind::Slice(_) => {
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // slice_ptr
                });
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // slice_argc
                });
            }
            _ => {
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // param
                });
            }
        }
    }

    let inventory = inventory_submit(&subr_name, &lisp_name, false, signatures);

    quote! {

        #inventory

        #[automatically_derived]
        #[doc(hidden)]
        #[inline(always)]
        unsafe fn #rust_wrapper_name(
            #(#c_params),*
        ) -> #wrapper_ret_ty {
            if cfg!(debug_assertions) {
                eprintln!("[DEBUG] Calling internal function: {}", #lisp_name);
                eprintln!("[DEBUG] Direct args: {}", stringify!(#(#c_params),*));
            }
            #(#arg_conversion)*
            if cfg!(debug_assertions) {
                eprintln!("[DEBUG] About to call internal Rust function: {}", stringify!(#subr));
            }
            let result = #subr(#(#call_args),*);
            if cfg!(debug_assertions) {
                eprintln!("[DEBUG] Internal Rust function {} returned", stringify!(#subr));
            }
            #wrapper_result
        }

        #extern_fn

        #body
    }
}

fn get_arg_conversion(args: &[Arg]) -> Vec<TokenStream> {
    let mut conversions = Vec::new();

    // Convert each argument directly from i64 parameter
    for (i, arg) in args.iter().enumerate() {
        let ident = &arg.ident;
        let conversion = match &arg.info.kind {
            ArgKind::Env => {
                quote! {
                    let #ident = env as *const crate::core::env::Environment;
                    let #ident = #ident.as_ref().ok_or("failed to convert env")?;
                }
            }
            ArgKind::Slice(inner_kind) => {
                let ptr_ident = format_ident!("{}_ptr", ident);
                let argc_ident = format_ident!("{}_argc", ident);

                match inner_kind.as_ref() {
                    ArgKind::Value => {
                        quote! {
                            let #ident = unsafe {
                                let ptr = #ptr_ident as *const crate::core::value::Value;
                                std::slice::from_raw_parts(ptr, #argc_ident as usize)
                            };
                        }
                    }
                    ArgKind::LispValue(ty) => {
                        quote! {
                            let mut slice_vec = Vec::with_capacity(#argc_ident as usize);
                            unsafe {
                                let ptr = #ptr_ident as *const i64;
                                for j in 0..#argc_ident as usize {
                                    let val = crate::core::value::Value(ptr.add(j).read() as u64);
                                    let converted: #ty = ::std::convert::TryFrom::try_from(val)?;
                                    slice_vec.push(converted);
                                }
                            }
                            let #ident = slice_vec.as_slice();
                        }
                    }
                    ArgKind::Primitive(ty) => {
                        quote! {
                            let #ident = unsafe {
                                let ptr = #ptr_ident as *const i64;
                                let new_slice = slice::from_raw_parts(ptr, #argc_ident);
                                new_slice
                            }
                        }
                    }
                    _ => {
                        quote! {
                            let #ident = unsafe {
                                let ptr = #ptr_ident as *const i64;
                                let new_slice = slice::from_raw_parts(ptr, #argc_ident);
                                new_slice
                            }
                        }
                    }
                }
            }
            ArgKind::Value => {
                if arg.info.is_ref {
                    let tmp = format_ident!("__arg_val_{}", i);
                    let mut_tok = if arg.info.is_mut {
                        quote! { mut }
                    } else {
                        quote! {}
                    };
                    let ref_tok = if arg.info.is_mut {
                        quote! { &mut #tmp }
                    } else {
                        quote! { &#tmp }
                    };
                    quote! {
                        let #mut_tok #tmp = crate::core::value::Value(#ident as u64);
                        let #ident = #ref_tok;
                    }
                } else {
                    quote! {
                        if cfg!(debug_assertions) {
                            eprintln!("[DEBUG] Converting internal arg {} from i64: 0x{:x}", stringify!(#ident), #ident as u64);
                        }
                        let #ident = crate::core::value::Value(#ident as u64);
                    }
                }
            }
            ArgKind::LispValue(ty) => {
                let mut_val = if arg.info.is_mut {
                    quote! { mut }
                } else {
                    quote! {}
                };
                quote! {
                    let #ident: &#mut_val #ty = TaggedPtr::untag_mut_checked(#ident as u64)?;
                }
            }
            ArgKind::Primitive(ty) => {
                quote! {
                    let #ident = #ident as #ty;
                }
            }
            _ => {
                quote! {
                    let #ident = ::std::convert::TryFrom::try_from(#ident).unwrap();
                    // let #ident = std::mem::transmute::<_, i64>(#ident);
                }
            }
        };

        conversions.push(conversion);
    }

    conversions
}

#[derive(Default, PartialEq, Debug, FromMeta)]
pub(crate) struct Spec {
    #[darling(default)]
    name: Option<String>,
}
