#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::function::{
    Arg, ArgKind, Function, RetKind, construct_objectref, construct_return,
    construct_return_nodrop,
};

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let lisp_name = spec.name.unwrap_or_else(|| map_function_name(&subr_name));
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let rust_wrapper_name = format_ident!("__rust_wrapper_{}", &subr_name);

    let args = function.args;
    // let arg_conversion = get_arg_conversion(&args);
    let (arg_conversion, arg_finish) = get_args(&args);

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

    let nil = quote! {
        crate::core::object::NIL
    };

    // let ret_is_unit = matches!(function.ret_kind, RetKind::Unit);
    let wrapper_ret_ty = quote! { ::std::result::Result<i64, &'static str> };

    // let forget_args = forget_args(&args);

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
        RetKind::Primitive(_ident) => construct_return(
            function.fallible,
            quote! {
                Ok(unsafe {val.to_raw() as i64})
            },
            quote! {
                Ok(unsafe {result.to_raw() as i64})
            },
        ),
        RetKind::Object => construct_return(
            function.fallible,
            construct_return_nodrop("val"),
            construct_return_nodrop("result"),
        ),
        RetKind::Option(inner) => {
            let some_to_value = match inner.as_ref() {
                RetKind::Object => quote! { v },
                // TODO
                RetKind::Primitive(_ty) => quote! {{
                    unsafe {v.to_raw() as i64}
                }},
                _ => panic!("invalid return type!"),
            };
            construct_return(
                function.fallible,
                quote! {
                    let ret = match val {
                        Some(v) => {
                            let __val = { #some_to_value };
                            __val.0 as i64
                        }
                        None => #nil as i64,
                    };
                    Ok(ret)
                },
                quote! {
                    let ret = match result {
                        Some(v) => {
                            let __val = { #some_to_value };
                            __val.0 as i64
                        }
                        None => #nil as i64,
                    };
                    Ok(ret)
                },
            )
        }
        _ => panic!("invalid return type"),
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
                tracing::trace!("[DEBUG] Calling defun function: {}", #lisp_name);
            }
            let args_cnt_u: usize = args_cnt as usize;
            #(#arg_conversion)*
            let result = #subr(#(#call_args),*);
            if cfg!(debug_assertions) {
                tracing::trace!("[DEBUG] Rust function {} returned", stringify!(#subr));
            }

            #(#arg_finish)*

            #wrapper_result
        }

        #extern_fn

        #body
    }
}

fn get_args(args: &[Arg]) -> (Vec<TokenStream>, Vec<TokenStream>) {
    let mut required_args: usize = 0;
    let mut _option_args: usize = 0;
    let mut has_slice = false;

    for (_i, arg) in args.iter().enumerate() {
        match &arg.info.kind {
            ArgKind::Slice(_) => {
                has_slice = true;
                break; // Slice must be last
            }
            ArgKind::Option(_) => {
                _option_args += 1;
                // Optional arguments don't count toward required
            }
            _ => {
                required_args += 1;
            }
        }
    }

    let object = quote! { crate::core::object::Object };
    let nil = quote! {
        crate::core::object::NIL
    };

    let mut init_args: Vec<TokenStream> = Vec::new();
    let mut finish_args: Vec<TokenStream> = Vec::new();

    // Add argument count validation
    if has_slice {
        init_args.push(quote! {
            if args_cnt_u < #required_args {
                return Err("insufficient number of arguments");
            }
        });
    } else {
        let max_args = args.len();
        init_args.push(quote! {
            if args_cnt_u < #required_args || args_cnt_u > #max_args {
                return Err("incorrect number of arguments");
            }
        });
    }
    let init = quote! {
        let env = env as *const crate::core::env::Environment;
        let env = env.as_ref().ok_or("failed to convert env")?;
    };
    init_args.push(init);

    for (i, arg) in args.iter().enumerate() {
        let ident = &arg.ident;
        let arg_info = &arg.info;
        let load_arg = quote! {
            let arg_val = unsafe {
                let ptr = args_ptr as *const i64;
                ptr.add(#i).read()
            };
        };
        let tmp = format_ident!("__arg_val_{}", i);
        match &arg_info.kind {
            ArgKind::Object => {
                let init = if arg_info.is_ref {
                    quote! {
                        #load_arg
                        let #tmp = #object(arg_val as u64);
                        let #ident = &#tmp;
                        env.stack_map.push(#ident);
                    }
                } else {
                    quote! {
                        #load_arg
                        let #ident = #object(arg_val as u64);
                        env.stack_map.push(&#ident);
                    }
                };
                let post = if arg_info.is_ref {
                    quote! {
                        env.stack_map.pop(&#tmp);
                        std::mem::forget(#tmp);
                    }
                } else {
                    quote! {}
                };
                finish_args.push(post);
                init_args.push(init);
            }
            ArgKind::ObjectRef(ty) => {
                let prog = construct_objectref(arg.info.is_mut, i, ident, ty);
                let init = quote! {
                    #load_arg
                    #prog
                    env.stack_map.push(&#tmp);
                };
                init_args.push(init);
                let post = quote! {
                    env.stack_map.pop(&#tmp);
                    std::mem::forget(#tmp);
                };
                finish_args.push(post);
            }
            ArgKind::Primitive(ty) => {
                let init = match &ty.to_string() as &str {
                    "Symbol" => {
                        quote! {
                            #load_arg
                            let #tmp = #object(arg_val as u64);
                            let #ident: #ty = (&#tmp).try_into()?;
                        }
                    }
                    "i64" | "u64" => {
                        quote! {
                            #load_arg
                            let #tmp = #object(arg_val as u64);
                            let #ident: crate::core::number::Integer = (&#tmp).try_into()?;
                            let #ident: #ty = #ident as #ty;
                        }
                    }
                    "f64" => {
                        quote! {
                            #load_arg
                            let #tmp = #object(arg_val as u64);
                            let #ident: crate::core::number::Float = (&#tmp).try_into()?;
                            let #ident: #ty = #ident as #ty;
                        }
                    }
                    "char" => {
                        quote! {
                            #load_arg
                            let #tmp = #object(arg_val as u64);
                            let #ident: crate::core::number::Character = (&#tmp).try_into()?;
                            let #ident: #ty = #ident as #ty;
                        }
                    }
                    _ => {
                        panic!("wrong primitive type!")
                    }
                };
                init_args.push(init);
            }
            ArgKind::Option(inner_kind) => {
                // Optional arguments - check if we have enough arguments
                let option_conversion = match inner_kind.as_ref() {
                    ArgKind::Object => {
                        if arg_info.is_ref {
                            quote! {
                                let (#tmp, #ident) = if args_cnt_u > #i {
                                    #load_arg
                                    let val = #object(arg_val as u64);
                                    env.stack_map.push(&val);
                                    (val, Some(&#tmp))
                                } else {
                                    (#object(#nil as u64), None)
                                };
                            }
                        } else {
                            quote! {
                                let #ident = if args_cnt_u > #i {
                                    let val = #object(arg_val as u64);
                                    env.stack_map.push(&val);
                                    Some(val)
                                } else {
                                    None
                                };
                            }
                        }
                    }
                    ArgKind::ObjectRef(ty) => {
                        let tmp_cast = format_ident!("__arg_cast_{}", i);
                        let ref_tok = if arg_info.is_mut {
                            quote! { &mut #tmp_cast }
                        } else {
                            quote! { &#tmp_cast }
                        };
                        let prog = construct_objectref(
                            arg.info.is_mut,
                            i,
                            &format_ident!("converted"),
                            ty,
                        );
                        quote! {
                            let (#tmp_cast, #ident) = if args_cnt_u > #i {
                                #load_arg
                                #prog
                                env.stack_map.push(&#tmp);
                                (converted, Some(#ref_tok))
                            } else {
                                (Default::default(), None)
                            };
                        }
                    }
                    _ => {
                        panic!("wrong type in option!")
                    }
                };
                // option_conversion
                init_args.push(option_conversion);
            }
            ArgKind::Slice(inner_kind) => {
                // Slice arguments consume all remaining arguments
                let slice_conversion = match inner_kind.as_ref() {
                    ArgKind::Object => {
                        quote! {
                            let slice_len = args_cnt_u - #i;
                            let #ident = unsafe {
                                let ptr = args_ptr as *const i64;
                                let slice_ptr = ptr.add(#i);
                                std::slice::from_raw_parts(slice_ptr as *const #object, slice_len)
                            };
                            for arg in #ident.iter() {
                                env.stack_map.push(arg);
                            }
                        }
                    }
                    _ => {
                        panic!("cannot convert: wrong arg type")
                    }
                };
                init_args.push(slice_conversion);

                let post = quote! {
                    for arg in #ident.iter() {
                        env.stack_map.pop(arg);
                    }
                };
                finish_args.push(post);
            }
            ArgKind::Env => (),
            _ => {
                panic!("invalid type!")
            }
        };
    }
    (init_args, finish_args)
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
