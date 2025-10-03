#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::function::{
    Arg, ArgKind, Function, RetKind, construct_objectref, construct_return, construct_return_nodrop,
};

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let lisp_name = spec.name.unwrap_or_else(|| subr_name.clone());
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let rust_wrapper_name = format_ident!("__rust_wrapper_{}", &subr_name);

    let args = function.args;
    let (arg_conversion, arg_finish) = get_args(&args);

    // Generate the extern "C" function signature and parameters
    let (c_params, c_param_idents) = get_c_signature(&args);

    // Generate the actual function call
    let call_args = args.iter().map(|arg| {
        let ident = &arg.ident;
        quote! { #ident }
    });

    let nil = quote! {
        crate::core::object::NIL
    };

    // let ret_is_unit = matches!(function.ret_kind, RetKind::Unit);
    let wrapper_ret_ty = quote! { ::std::result::Result<i64, &'static str> };

    let wrapper_result = match &function.ret_kind {
        RetKind::Unit => construct_return(
            function.fallible,
            quote! {
                Ok(#nil as i64)
            },
            quote! {
                Ok(#nil as i64)
            },
        ),
        RetKind::Object => construct_return(
            function.fallible,
            construct_return_nodrop("val"),
            construct_return_nodrop("result"),
        ),
        RetKind::Primitive(ident) if ident == "Symbol" => construct_return(
            function.fallible,
            quote! {
                Ok(unsafe {val.to_raw() as i64})
            },
            quote! {
                Ok(unsafe {result.to_raw() as i64})
            },
        ),
        RetKind::Primitive(_ident) => construct_return(
            function.fallible,
            quote! {
                Ok(val as i64)
            },
            quote! {
                Ok(result as i64)
            },
        ),
        RetKind::Option(inner) => {
            let some_to_value = match inner.as_ref() {
                RetKind::Object => quote! { v },
                _ => quote! {{
                    let vv: crate::core::object::Object = ::std::convert::Into::into(v);
                    vv
                }},
            };
            construct_return(
                function.fallible,
                quote! {
                    let ret = match val {
                        Some(v) => {
                            let __val = { #some_to_value };
                            let return_val = __val.0;
                            std::mem::forget(__val);
                            return_val as i64
                        }
                        None => #nil as i64,
                    };
                    Ok(ret)
                },
                quote! {
                    let ret = match result {
                        Some(v) => {
                            let __val = { #some_to_value };
                            let return_val = __val.0;
                            std::mem::forget(__val);
                            return_val as i64
                        }
                        None => #nil as i64,
                    };
                    Ok(ret)
                },
            )
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
                quote! { Ok(#val) }
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
                    #nil as i64
                }
            }
        }
    };

    let signatures = get_signatures(&args);
    let inventory = inventory_submit(&subr_name, &lisp_name, signatures);

    quote! {

        #inventory

        #[automatically_derived]
        #[doc(hidden)]
        #[inline(always)]
        unsafe fn #rust_wrapper_name(
            #(#c_params),*
        ) -> #wrapper_ret_ty {
            if cfg!(debug_assertions) {
                tracing::trace!("[DEBUG] Calling internal function: {}", #lisp_name);
            }
            #(#arg_conversion)*
            let result = #subr(#(#call_args),*);
            if cfg!(debug_assertions) {
                tracing::trace!("[DEBUG] Internal Rust function {} returned", stringify!(#subr));
            }

            #(#arg_finish)*
            #wrapper_result
        }

        #extern_fn

        #body
    }
}

fn get_c_signature(args: &[Arg]) -> (Vec<TokenStream>, Vec<proc_macro2::Ident>) {
    let mut c_params: Vec<TokenStream> = Vec::new();
    let mut c_param_idents: Vec<proc_macro2::Ident> = Vec::new();

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

    (c_params, c_param_idents)
}

fn get_args(args: &[Arg]) -> (Vec<TokenStream>, Vec<TokenStream>) {
    let mut init_args: Vec<TokenStream> = Vec::new();
    let mut finish_args: Vec<TokenStream> = Vec::new();

    let object = quote! { crate::core::object::Object };

    // Convert each argument directly from i64 parameter
    for (i, arg) in args.iter().enumerate() {
        let ident = &arg.ident;
        let tmp = format_ident!("__arg_val_{}", i);
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
                    ArgKind::Object => {
                        quote! {
                            let #ident = unsafe {
                                let ptr = #ptr_ident as *const #object;
                                std::slice::from_raw_parts(ptr, #argc_ident as usize)
                            };
                        }
                    }
                    ArgKind::ObjectRef(_) => {
                        panic!("you should not use ObjectRef in a slice!");
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
            ArgKind::Object => {
                if arg.info.is_ref {
                    quote! {
                        let #tmp = #object(#ident as u64);
                        let #ident = &#tmp;
                    }
                } else {
                    quote! {
                        if cfg!(debug_assertions) {
                            tracing::trace!("[DEBUG] Converting internal arg {} from i64: 0x{:x}", stringify!(#ident), #ident as u64);
                        }
                        let #ident = #object(#ident as u64);
                    }
                }
            }
            ArgKind::ObjectRef(ty) => construct_objectref(arg.info.is_mut, i, ident, ty),
            ArgKind::Primitive(ty) => {
                if ty.to_string() == "Symbol" {
                    quote! {
                        let #tmp = #object(#ident as u64);
                        let #ident: #ty = (&#tmp).try_into()?;
                    }
                } else {
                    quote! {
                        let #ident = #ident as #ty;
                    }
                }
            }
            _ => {
                quote! {
                    let #ident = ::std::convert::TryFrom::try_from(#ident).unwrap();
                }
            }
        };

        init_args.push(conversion);

        // Add cleanup for objects that need it
        let cleanup = match &arg.info.kind {
            ArgKind::Object => {
                if arg.info.is_ref {
                    quote! {
                        std::mem::forget(#tmp);
                    }
                } else {
                    quote! {}
                }
            }
            ArgKind::ObjectRef(_) => {
                if arg.info.is_ref {
                    quote! {
                        std::mem::forget(#tmp);
                    }
                } else {
                    quote! {}
                }
            }
            ArgKind::Primitive(ty) if ty.to_string() == "Symbol" => {
                quote! {
                    std::mem::forget(#tmp);
                }
            }
            _ => quote! {},
        };
        finish_args.push(cleanup);
    }

    (init_args, finish_args)
}

fn get_signatures(args: &[Arg]) -> Vec<TokenStream> {
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
    signatures
}

#[derive(Default, PartialEq, Debug, FromMeta)]
pub(crate) struct Spec {
    #[darling(default)]
    name: Option<String>,
}

pub fn inventory_submit(
    subr_name: &str,
    lisp_name: &str,
    signatures: Vec<TokenStream>,
) -> TokenStream {
    let subr_name = subr_name.to_string();
    let lisp_name = lisp_name.to_string();
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let def_func_name = format_ident!("__def_{}", &subr_name);
    let register_func_name = format_ident!("__register_{}", &subr_name);

    let mut return_sigs: Vec<TokenStream> = Vec::new();
    return_sigs.push(quote! {
        sig.returns.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64));
    });

    quote! {
        #[automatically_derived]
        #[doc(hidden)]
        fn #def_func_name<T: cranelift_module::Module>(module: &mut T) -> anyhow::Result<(String, cranelift_module::FuncId)> {
            let mut sig = module.make_signature();
            #(#signatures)*
            #(#return_sigs)*

            let func_id = module.declare_function(#lisp_name, cranelift_module::Linkage::Import, &sig)?;
            let func_name = #lisp_name.to_string();
            Ok((func_name, func_id))
        }

        #[automatically_derived]
        #[doc(hidden)]
        fn #register_func_name(jit_builder: &mut cranelift_jit::JITBuilder) {
            tracing::info!(
                "adding internal function {}({}): {:?}",
                stringify!(#subr_name),
                stringify!(#func_name),
                #func_name as *const u8
            );
            jit_builder.symbol(#lisp_name, #func_name as *const u8);
        }

        inventory::submit!(crate::core::compiler::InternalFnPlugin::new(
            #def_func_name,
            #register_func_name,
            #func_name as *const u8
        ));
    }
}
