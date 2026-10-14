#![allow(clippy::manual_unwrap_or_default)]
use std::panic;

use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::function::{
    Arg, ArgKind, Function, RetKind, calculate_signature, construct_return, construct_return_nodrop,
};

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    let args = function.args;
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let lisp_name = spec.name.unwrap_or_else(|| map_function_name(&subr_name));
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);

    let (use_argv_abi, arg_conversion, arg_finish) = get_args(&args);

    // Generate the extern "C" function signature
    let (c_params, _c_param_idents) = if !use_argv_abi {
        get_direct_c_signature(&args)
    } else {
        (
            vec![
                quote! { args_ptr: i64 },
                quote! { args_cnt: i64 },
                quote! { env: i64 },
                quote! { _dynamic: i64 },
                quote! { _lexical: i64 },
            ],
            vec![
                format_ident!("args_ptr"),
                format_ident!("args_cnt"),
                format_ident!("env"),
                format_ident!("_dynamic"),
                format_ident!("_lexical"),
            ],
        )
    };

    // Generate the actual function call
    let call_args = args.iter().map(|arg| {
        let ident = &arg.ident;
        quote! { #ident }
    });

    let nil = quote! {
        crate::core::object::NIL
    };

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
            macro_rules! __unwrap_or_nil {
                ($expr:expr) => {
                    match $expr {
                        Ok(v) => v,
                        Err(e) => {
                            tracing::error!("error: {e:?}");
                            return #nil as i64;
                        }
                    }
                };
            }

            if cfg!(debug_assertions) {
                tracing::trace!("[DEBUG] Calling defun function: {}", #lisp_name);
            }
            #(#arg_conversion)*
            let result = #subr(#(#call_args),*);
            if cfg!(debug_assertions) {
                tracing::trace!("[DEBUG] Rust function {} returned", stringify!(#subr));
            }

            #(#arg_finish)*

            let __wrapper_result: ::std::result::Result<i64, &'static str> = {
                #wrapper_result
            };
            match __wrapper_result {
                Ok(v) => v,
                Err(e) => {
                    tracing::error!("error: {e:?}");
                    #nil as i64
                }
            }
        }
    };

    let signatures = if !use_argv_abi {
        get_direct_signatures(&args)
    } else {
        vec![
            quote! {
                sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // args_ptr
            },
            quote! {
                sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // args_cnt
            },
            quote! {
                sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // env
            },
            quote! {
                sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // dynamic
            },
            quote! {
                sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // lexical
            },
        ]
    };

    let inventory = inventory_submit(&subr_name, &lisp_name, signatures, &args);

    quote! {

        #inventory

        #extern_fn

        #body
    }
}

fn get_direct_c_signature(args: &[Arg]) -> (Vec<TokenStream>, Vec<proc_macro2::Ident>) {
    let mut c_params: Vec<TokenStream> = Vec::new();
    let mut c_param_idents: Vec<proc_macro2::Ident> = Vec::new();

    for arg in args.iter() {
        let arg_info = &arg.info;
        let ident = &arg.ident;
        match &arg_info.kind {
            ArgKind::Env | ArgKind::GcContext => {
                // Skip - will be added at the end
            }
            ArgKind::Slice(_) | ArgKind::RootedSlice => {
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

    // Always add env at the end
    c_params.push(quote! { env: i64 });
    c_param_idents.push(format_ident!("env"));
    c_params.push(quote! { _dynamic: i64 });
    c_param_idents.push(format_ident!("_dynamic"));
    c_params.push(quote! { _lexical: i64 });
    c_param_idents.push(format_ident!("_lexical"));

    (c_params, c_param_idents)
}

fn get_direct_signatures(args: &[Arg]) -> Vec<TokenStream> {
    let mut signatures: Vec<TokenStream> = Vec::new();

    for arg in args.iter() {
        match &arg.info.kind {
            ArgKind::Env | ArgKind::GcContext => {
                // Skip - will be added at the end
            }
            ArgKind::Slice(_) | ArgKind::RootedSlice => {
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // slice_ptr
                });
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // slice_argc
                });
                // Rest args use the argv ABI.
            }
            _ => {
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // param
                });
            }
        }
    }

    // Always add env at the end
    signatures.push(quote! {
        sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // env
    });
    signatures.push(quote! {
        sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // dynamic
    });
    signatures.push(quote! {
        sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // lexical
    });

    signatures
}

fn get_args(args: &[Arg]) -> (bool, Vec<TokenStream>, Vec<TokenStream>) {
    let (normal, optional, rest) = calculate_signature(args);

    let use_argv_abi = rest || { normal + optional > 8 };

    let object = quote! { crate::core::object::Object };
    let rooted_slice = quote! { crate::gc::RootedSlice };
    let has_gc_context = args
        .iter()
        .any(|arg| matches!(arg.info.kind, ArgKind::GcContext));
    let has_rooted_args = args.iter().any(|arg| {
        matches!(arg.info.kind, ArgKind::Rooted | ArgKind::RootedSlice)
            || matches!(&arg.info.kind, ArgKind::Slice(inner) if matches!(inner.as_ref(), ArgKind::Rooted))
    });
    let mut init_args: Vec<TokenStream> = Vec::new();
    let mut finish_args: Vec<TokenStream> = Vec::new();
    let mut context_binding: Option<TokenStream> = None;

    if has_rooted_args && !has_gc_context {
        panic!("Rooted arguments in #[defun] require &mut GcContext");
    }

    if has_gc_context {
        let context = args
            .iter()
            .find(|arg| matches!(arg.info.kind, ArgKind::GcContext))
            .expect("GcContext argument disappeared");
        if !context.info.is_ref {
            panic!("GcContext arguments in #[defun] must be borrowed");
        }
        if has_rooted_args && !context.info.is_mut {
            panic!("Rooted arguments in #[defun] require &mut GcContext");
        }
        init_args.push(quote! {
            let mut __gc_context = crate::gc::GcContext::new();
        });
    }

    if use_argv_abi {
        init_args.push(quote! {
            let args_cnt: usize = args_cnt as usize;
        });
    }

    let env_init = quote! {
        let env = env as *const crate::core::env::Environment;
        let env = __unwrap_or_nil!(env.as_ref().ok_or("failed to convert env"));
    };
    init_args.push(env_init);

    for (i, arg) in args.iter().enumerate() {
        let ident = &arg.ident;
        let arg_info = &arg.info;

        let load_arg = if use_argv_abi {
            quote! {
                let arg_val = unsafe {
                    let ptr = args_ptr as *const i64;
                    ptr.add(#i).read()
                };
            }
        } else {
            quote! {
                    let arg_val = #ident;
            }
        };

        let tmp = format_ident!("__arg_val_{}", i);
        match &arg_info.kind {
            ArgKind::Object => {
                if has_gc_context {
                    panic!(
                        "&Object arguments in #[defun] with GcContext must be rewritten to &Rooted"
                    );
                }
                if !arg_info.is_ref || arg_info.is_mut {
                    panic!("Object arguments in #[defun] must be borrowed as &Object");
                }
                let init = quote! {
                    #load_arg
                    let #tmp = #object(arg_val as u64);
                    let #ident = &#tmp;
                };
                init_args.push(init);
                finish_args.push(quote! {});
            }
            ArgKind::Rooted => {
                if !arg_info.is_ref {
                    panic!("Rooted arguments must be passed by reference");
                }
                let init = quote! {
                    #load_arg
                    let #tmp = __gc_context.root(#object(arg_val as u64));
                    let #ident = &#tmp;
                };
                init_args.push(init);
                finish_args.push(quote! {});
            }
            ArgKind::RootedSlice => {
                if !arg_info.is_ref || arg_info.is_mut {
                    panic!("RootedSlice arguments must be borrowed as &RootedSlice");
                }
                let ptr_ident = format_ident!("{}_ptr", ident);
                let argc_ident = format_ident!("{}_argc", ident);
                let source = if use_argv_abi {
                    quote! {
                        let __slice_ptr = (args_ptr as *const #object).add(#i);
                        let __slice_len = args_cnt - #i;
                    }
                } else {
                    quote! {
                        let __slice_ptr = #ptr_ident as *const #object;
                        let __slice_len = #argc_ident as usize;
                    }
                };
                init_args.push(quote! {
                    let #tmp: #rooted_slice = unsafe {
                        #source
                        let __slice = ::std::slice::from_raw_parts(__slice_ptr, __slice_len);
                        __gc_context.root_slice(__slice)
                    };
                    let #ident = &#tmp;
                });
            }
            ArgKind::ObjectRef(ty) => {
                if has_gc_context {
                    panic!(
                        "heap reference arguments in #[defun] with GcContext must be rewritten to &Rooted"
                    );
                }
                if !arg_info.is_ref || arg_info.is_mut {
                    panic!("heap reference arguments in #[defun] must be borrowed as &T");
                }
                let init = quote! {
                    #load_arg
                    let #tmp = #object(arg_val as u64);
                    let #ident: &#ty = __unwrap_or_nil!((&#tmp).try_into());
                };
                init_args.push(init);
                finish_args.push(quote! {});
            }
            ArgKind::Primitive(ty) => {
                let init = match &ty.to_string() as &str {
                    "Symbol" => {
                        quote! {
                            #load_arg
                            let #tmp = #object(arg_val as u64);
                            let #ident: #ty = __unwrap_or_nil!((&#tmp).try_into());
                        }
                    }
                    "i64" | "u64" => {
                        quote! {
                            #load_arg
                            let #tmp = #object(arg_val as u64);
                            let #ident: crate::core::number::Integer = __unwrap_or_nil!((&#tmp).try_into());
                            let #ident: #ty = #ident as #ty;
                        }
                    }
                    "f64" => {
                        quote! {
                            #load_arg
                            let #tmp = #object(arg_val as u64);
                            let #ident: crate::core::number::Float = __unwrap_or_nil!((&#tmp).try_into());
                            let #ident: #ty = #ident as #ty;
                        }
                    }
                    "char" => {
                        quote! {
                            #load_arg
                            let #tmp = #object(arg_val as u64);
                            let #ident: crate::core::number::Character = __unwrap_or_nil!((&#tmp).try_into());
                            let #ident: #ty = #ident as #ty;
                        }
                    }
                    _ => {
                        panic!("wrong primitive type!")
                    }
                };
                init_args.push(init);
            }
            ArgKind::Option(_inner_kind) => {
                panic!("optional arguments in #[defun] must be rewritten for rooted calling")
            }
            ArgKind::Slice(_) => {
                panic!("Rust slice arguments in #[defun] must use &RootedSlice")
            }
            ArgKind::GcContext => {
                let binding = if arg_info.is_mut {
                    quote! {
                        let #ident = &mut __gc_context;
                    }
                } else {
                    quote! {
                        let #ident = &__gc_context;
                    }
                };
                context_binding = Some(binding);
            }
            ArgKind::Env => (),
            _ => {
                panic!("invalid type!")
            }
        };
    }
    if let Some(binding) = context_binding {
        init_args.push(binding);
    }
    (use_argv_abi, init_args, finish_args)
}

#[derive(Default, PartialEq, Debug, FromMeta)]
pub(crate) struct Spec {
    #[darling(default)]
    name: Option<String>,
    #[darling(default)]
    required: Option<u16>,
}

fn map_function_name(name: &str) -> String {
    name.replace('_', "-")
}

pub fn inventory_submit(
    subr_name: &str,
    lisp_name: &str,
    signatures: Vec<TokenStream>,
    args: &[crate::function::Arg],
) -> TokenStream {
    let subr_name = subr_name.to_string();
    let lisp_name = lisp_name.to_string();
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let def_func_name = format_ident!("__def_{}", &subr_name);
    let register_func_name = format_ident!("__register_{}", &subr_name);

    // Calculate signature
    let (normal, optional, rest) = crate::function::calculate_signature(args);

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
                "adding native function {}({}): {:?}",
                stringify!(#subr_name),
                stringify!(#func_name),
                #func_name as *const u8
            );

            jit_builder.symbol(#lisp_name, #func_name as *const u8);

        }

        inventory::submit!(crate::core::compiler::BuiltinFnPlugin::new(
            #def_func_name,
            #register_func_name,
            #func_name as *const u8,
            crate::core::function::FunctionSignature {
                normal: #normal,
                optional: #optional,
                rest: #rest,
            }
        ));

    }
}
