#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use syn::{Error, Type};

use crate::function::{Arg, ArgKind, Function, RetKind};

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let lisp_name = spec.name.unwrap_or_else(|| map_function_name(&subr_name));
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let rust_wrapper_name = format_ident!("__rust_wrapper_{}", &subr_name);

    let args = function.args;
    let arg_conversion = get_arg_conversion(&args);
    let is_lisp_subr = spec.is_lisp_subr && function.is_lisp_subr;

    // Generate the extern "C" function signature
    let c_params = vec![
        quote! { args_ptr: i64 },
        quote! { args_cnt: i64 },
        quote! { env: i64 },
    ];
    let c_param_idents = vec![
        format_ident!("args_ptr"),
        format_ident!("args_cnt"),
        format_ident!("env"),
    ];

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
                        Ok(val) => Ok(val),
                        Err(_) => Err("call error"),
                    }
                }
            } else {
                quote! {
                    Ok(result)
                }
            }
        }
        RetKind::Value => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(val) => Ok(val.0 as i64),
                        Err(_) => Err("call error"),
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
                        Err(_) => Err("call error"),
                    }
                }
            } else {
                quote! {
                    // TODO check if this is correct
                    let v: crate::core::value::Value = result.tag();
                    Ok(v.0 as i64)
                }
            }
        }
        RetKind::Option(inner) => {
            let some_to_value = match inner.as_ref() {
                RetKind::Value => quote! { v },
                RetKind::IntoValue => quote! {{
                    let vv: crate::core::value::Value = v.tag();
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
                        Err(_) => Err("call error"),
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
        RetKind::Primitive(_ident) => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(val) => Ok(val as i64),
                        Err(_) => Err("call error"),
                    }
                }
            } else {
                quote! { Ok(result as i64) }
            }
        }
        RetKind::Other => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(val) => Ok(val as *const u8 as i64),
                        Err(_) => Err("call error"),
                    }
                }
            } else {
                quote! { Ok(result as *const u8 as i64) }
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

    let signatures = vec![
        quote! {
            sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // args_ptr
        },
        quote! {
            sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // args_cnt
        },
        quote! {
            sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // env
        },
    ];

    let inventory = inventory_submit(&subr_name, &lisp_name, is_lisp_subr, signatures);

    quote! {

        #inventory

        #[automatically_derived]
        #[doc(hidden)]
        #[inline(always)]
        unsafe fn #rust_wrapper_name(
            #(#c_params),*
        ) -> #wrapper_ret_ty {
            if cfg!(debug_assertions) {
                eprintln!("[DEBUG] Calling defun function: {}", #lisp_name);
                eprintln!("[DEBUG] args_ptr: 0x{:x}, args_cnt: {}", args_ptr, args_cnt);
            }
            let args_cnt_u: usize = args_cnt as usize;
            #(#arg_conversion)*
            if cfg!(debug_assertions) {
                eprintln!("[DEBUG] About to call Rust function: {}", stringify!(#subr));
            }
            let result = #subr(#(#call_args),*);
            if cfg!(debug_assertions) {
                eprintln!("[DEBUG] Rust function {} returned", stringify!(#subr));
            }
            #wrapper_result
        }

        #extern_fn

        #body
    }
}

fn get_arg_conversion(args: &[Arg]) -> Vec<TokenStream> {
    let mut conversions = Vec::new();

    // Find the minimum required arguments (non-optional, non-slice)
    let mut required_args: usize = 0;
    let mut has_slice = false;

    for (_i, arg) in args.iter().enumerate() {
        match &arg.info.kind {
            ArgKind::Slice(_) => {
                has_slice = true;
                break; // Slice must be last
            }
            ArgKind::Option(_) => {
                // Optional arguments don't count toward required
            }
            _ => {
                required_args += 1;
            }
        }
    }

    // Add argument count validation
    if has_slice {
        conversions.push(quote! {
            if args_cnt_u < #required_args {
                return Err("insufficient number of arguments");
            }
        });
    } else {
        let max_args = args.len();
        conversions.push(quote! {
            if args_cnt_u < #required_args || args_cnt_u > #max_args {
                return Err("incorrect number of arguments");
            }
        });
    }

    // Convert each argument by loading from the args array
    for (i, arg) in args.iter().enumerate() {
        let ident = &arg.ident;
        let arg_info = &arg.info;
        let conversion = match &arg_info.kind {
            ArgKind::Env => {
                quote! {
                    let #ident = env as *const crate::core::env::Environment;
                    let #ident = #ident.as_ref().ok_or("failed to convert env")?;
                }
            }
            ArgKind::Slice(inner_kind) => {
                // Slice arguments consume all remaining arguments
                let slice_conversion = match inner_kind.as_ref() {
                    ArgKind::Value => {
                        quote! {
                            let slice_len = args_cnt_u - #i;
                            if cfg!(debug_assertions) {
                                eprintln!("[DEBUG] Creating Value slice of length {} starting at arg {}", slice_len, #i);
                            }
                            let #ident = unsafe {
                                let ptr = args_ptr as *const i64;
                                let slice_ptr = ptr.add(#i);
                                std::slice::from_raw_parts(slice_ptr as *const crate::core::value::Value, slice_len)
                            };
                        }
                    }
                    ArgKind::FromValue(ty) => {
                        // For FromValue slices, we need to convert each element
                        quote! {
                            let slice_len = args_cnt_u - #i;
                            let mut slice_vec = Vec::with_capacity(slice_len);
                            unsafe {
                                let ptr = args_ptr as *const i64;
                                for j in 0..slice_len {
                                    let arg_val = ptr.add(#i + j).read();
                                    let val = crate::core::value::Value(arg_val as u64);
                                    let converted: #ty = ::std::convert::TryFrom::try_from(val)?;
                                    slice_vec.push(converted);
                                }
                            }
                            let #ident = slice_vec.as_slice();
                        }
                    }
                    // ArgKind::Other(ty) => {
                    //     quote! {
                    //         let slice_len = args_cnt_u - #i;
                    //         let mut slice_vec = Vec::with_capacity(slice_len);
                    //         unsafe {
                    //             let ptr = args_ptr as *const i64;
                    //             for j in 0..slice_len {
                    //                 let arg_val = ptr.add(#i + j).read();
                    //                 let converted: #ty = ::std::convert::TryFrom::try_from(arg_val)?;
                    //                 slice_vec.push(converted);
                    //             }
                    //         }
                    //         let #ident = slice_vec.as_slice();
                    //     }
                    // }
                    _ => {
                        panic!("cannot convert: wrong arg type")
                    }
                };
                slice_conversion
            }
            ArgKind::Option(inner_kind) => {
                // Optional arguments - check if we have enough arguments
                let option_conversion = match inner_kind.as_ref() {
                    ArgKind::Value => {
                        if arg_info.is_ref {
                            let tmp = format_ident!("__arg_val_{}", i);
                            let mut_tok = if arg_info.is_mut {
                                quote! { mut }
                            } else {
                                quote! {}
                            };
                            let ref_tok = if arg_info.is_mut {
                                quote! { &mut #tmp }
                            } else {
                                quote! { &#tmp }
                            };
                            quote! {
                                let (#mut_tok #tmp, #ident) = if args_cnt_u > #i {
                                    let arg_val = unsafe {
                                        let ptr = args_ptr as *const i64;
                                        ptr.add(#i).read()
                                    };
                                    let val = crate::core::value::Value(arg_val as u64);
                                    (val, Some(#ref_tok))
                                } else {
                                    (crate::core::value::Value(crate::core::value::NIL as u64), None)
                                };
                            }
                        } else {
                            quote! {
                                let #ident = if args_cnt_u > #i {
                                    let arg_val = unsafe {
                                        let ptr = args_ptr as *const i64;
                                        ptr.add(#i).read()
                                    };
                                    Some(crate::core::value::Value(arg_val as u64))
                                } else {
                                    None
                                };
                            }
                        }
                    }
                    ArgKind::FromValue(ty) => {
                        if arg_info.is_ref {
                            let tmp_val = format_ident!("__arg_val_{}", i);
                            let tmp_cast = format_ident!("__arg_cast_{}", i);
                            let mut_val = if arg_info.is_mut {
                                quote! { mut }
                            } else {
                                quote! {}
                            };
                            let ref_tok = if arg_info.is_mut {
                                quote! { &mut #tmp_cast }
                            } else {
                                quote! { &#tmp_cast }
                            };
                            quote! {
                                let (#mut_val #tmp_cast, #ident) = if args_cnt_u > #i {
                                    let arg_val = unsafe {
                                        let ptr = args_ptr as *const i64;
                                        ptr.add(#i).read()
                                    };
                                    let #mut_val #tmp_val = crate::core::value::Value(arg_val as u64);
                                    let converted: #ty = ::std::convert::TryFrom::try_from(#tmp_val)?;
                                    (converted, Some(#ref_tok))
                                } else {
                                    (Default::default(), None)
                                };
                            }
                        } else {
                            quote! {
                                let #ident = if args_cnt_u > #i {
                                    let arg_val = unsafe {
                                        let ptr = args_ptr as *const i64;
                                        ptr.add(#i).read()
                                    };
                                    let val = crate::core::value::Value(arg_val as u64);
                                    let converted: #ty = ::std::convert::TryFrom::try_from(val)?;
                                    Some(converted)
                                } else {
                                    None
                                };
                            }
                        }
                    }
                    _ => {
                        panic!("wrong type in option!")
                    }
                };
                option_conversion
            }
            ArgKind::Value => {
                let load_arg = quote! {
                    let arg_val = unsafe {
                        let ptr = args_ptr as *const i64;
                        if cfg!(debug_assertions) {
                            eprintln!("[DEBUG] Loading arg {} from ptr: 0x{:x} + {}", #i, ptr as usize, #i);
                        }
                        ptr.add(#i).read()
                    };
                    if cfg!(debug_assertions) {
                        eprintln!("[DEBUG] Loaded arg {} value: 0x{:x}", #i, arg_val as u64);
                    }
                };

                if arg_info.is_ref {
                    let tmp = format_ident!("__arg_val_{}", i);
                    let mut_tok = if arg_info.is_mut {
                        quote! { mut }
                    } else {
                        quote! {}
                    };
                    let ref_tok = if arg_info.is_mut {
                        quote! { &mut #tmp }
                    } else {
                        quote! { &#tmp }
                    };
                    quote! {
                        #load_arg
                        let #mut_tok #tmp = crate::core::value::Value(arg_val as u64);
                        let #ident = #ref_tok;
                    }
                } else {
                    quote! {
                        #load_arg
                        let #ident = crate::core::value::Value(arg_val as u64);
                    }
                }
            }
            ArgKind::FromValue(ty) => {
                let load_arg = quote! {
                    let arg_val = unsafe {
                        let ptr = args_ptr as *const i64;
                        ptr.add(#i).read()
                    };
                };

                if arg_info.is_ref {
                    let tmp_val = format_ident!("__arg_val_{}", i);
                    let tmp_cast = format_ident!("__arg_cast_{}", i);
                    let mut_val = if arg_info.is_mut {
                        quote! { mut }
                    } else {
                        quote! {}
                    };
                    let ref_tok = if arg_info.is_mut {
                        quote! { &mut #tmp_cast }
                    } else {
                        quote! { &#tmp_cast }
                    };
                    quote! {
                        #load_arg
                        let #mut_val #tmp_val = crate::core::value::Value(arg_val as u64);
                        let #mut_val #tmp_cast: #ty = ::std::convert::TryFrom::try_from(#tmp_val)?;
                        let #ident = #ref_tok;
                    }
                } else {
                    quote! {
                        #load_arg
                        let #ident: #ty = ::std::convert::TryFrom::try_from(crate::core::value::Value(arg_val as u64))?;
                    }
                }
            }
            _ => {
                panic!("invalid type!")
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
    #[darling(default)]
    required: Option<u16>,
    #[darling(default = "tru")]
    is_lisp_subr: bool,
}

fn tru() -> bool {
    true
}

fn map_function_name(name: &str) -> String {
    name.replace('_', "-")
}

pub fn inventory_submit(
    subr_name: &str,
    lisp_name: &str,
    is_lisp_subr: bool,
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

            jit_builder.symbol(#lisp_name, #func_name as *const u8);

        }

        inventory::submit!(crate::core::compiler::BuiltinFnPlugin::new(#def_func_name, #register_func_name, #is_lisp_subr, #func_name as *const u8));

    }
}