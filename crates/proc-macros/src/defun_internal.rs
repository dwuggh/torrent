#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::function::{Arg, ArgKind, Function, RetKind, construct_return, construct_return_nodrop};

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    let args = function.args;
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let lisp_name = spec.name.unwrap_or_else(|| subr_name.clone());
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);

    let (arg_conversion, arg_finish) = get_args(&args);

    // Generate the extern "C" function signature and parameters
    let (c_params, _c_param_idents) = get_c_signature(&args);

    // Generate the actual function call
    let call_args = args.iter().map(|arg| {
        let ident = &arg.ident;
        quote! { #ident }
    });

    let nil = quote! {
        crate::core::object::NIL
    };

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
                tracing::trace!("[DEBUG] Calling internal function: {}", #lisp_name);
            }
            #(#arg_conversion)*
            let result = #subr(#(#call_args),*);
            if cfg!(debug_assertions) {
                tracing::trace!("[DEBUG] Internal Rust function {} returned", stringify!(#subr));
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

    let signatures = get_signatures(&args);
    let inventory = inventory_submit(&subr_name, &lisp_name, signatures);

    quote! {

        #inventory

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
            ArgKind::GcContext => {}
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

    (c_params, c_param_idents)
}

fn get_args(args: &[Arg]) -> (Vec<TokenStream>, Vec<TokenStream>) {
    let mut init_args: Vec<TokenStream> = Vec::new();
    let mut finish_args: Vec<TokenStream> = Vec::new();
    let mut context_binding: Option<TokenStream> = None;

    let object = quote! { crate::core::object::Object };
    let rooted_slice = quote! { crate::gc::RootedSlice };
    let has_gc_context = args
        .iter()
        .any(|arg| matches!(arg.info.kind, ArgKind::GcContext));
    let has_rooted_args = args.iter().any(|arg| {
        matches!(arg.info.kind, ArgKind::Rooted | ArgKind::RootedSlice)
            || matches!(&arg.info.kind, ArgKind::Slice(inner) if matches!(inner.as_ref(), ArgKind::Rooted))
    });

    if has_rooted_args && !has_gc_context {
        panic!("Rooted arguments in #[internal_fn] require &mut GcContext");
    }

    if has_gc_context {
        let context = args
            .iter()
            .find(|arg| matches!(arg.info.kind, ArgKind::GcContext))
            .expect("GcContext argument disappeared");
        if !context.info.is_ref {
            panic!("GcContext arguments in #[internal_fn] must be borrowed");
        }
        if has_rooted_args && !context.info.is_mut {
            panic!("Rooted arguments in #[internal_fn] require &mut GcContext");
        }
        init_args.push(quote! {
            let mut __gc_context = crate::gc::GcContext::new();
        });
    }

    // Convert each argument directly from i64 parameter
    for (i, arg) in args.iter().enumerate() {
        let ident = &arg.ident;
        let tmp = format_ident!("__arg_val_{}", i);
        let conversion = match &arg.info.kind {
            ArgKind::Env => {
                quote! {
                    let #ident = env as *const crate::core::env::Environment;
                    let #ident = __unwrap_or_nil!(#ident.as_ref().ok_or("failed to convert env"));
                }
            }
            ArgKind::Slice(inner_kind) => {
                let ptr_ident = format_ident!("{}_ptr", ident);
                let argc_ident = format_ident!("{}_argc", ident);

                match inner_kind.as_ref() {
                    ArgKind::Object => {
                        panic!(
                            "raw Object slices in #[internal_fn] must be rewritten to &RootedSlice"
                        )
                    }
                    ArgKind::Rooted => {
                        panic!("&[Rooted] in #[internal_fn] must be rewritten to &RootedSlice")
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
                if has_gc_context {
                    panic!(
                        "&Object arguments in #[internal_fn] with GcContext must be rewritten to &Rooted"
                    );
                }
                if !arg.info.is_ref || arg.info.is_mut {
                    panic!("Object arguments in #[internal_fn] must be borrowed as &Object");
                }
                quote! {
                    if cfg!(debug_assertions) {
                        tracing::trace!("[DEBUG] Converting internal arg {} from i64: 0x{:x}", stringify!(#ident), #ident as u64);
                    }
                    let #tmp = #object(#ident as u64);
                    let #ident = &#tmp;
                }
            }
            ArgKind::Rooted => {
                if !arg.info.is_ref {
                    panic!("Rooted arguments must be passed by reference");
                }
                quote! {
                    let #tmp = __gc_context.root(#object(#ident as u64));
                    let #ident = &#tmp;
                }
            }
            ArgKind::RootedSlice => {
                if !arg.info.is_ref || arg.info.is_mut {
                    panic!("RootedSlice arguments must be borrowed as &RootedSlice");
                }
                let ptr_ident = format_ident!("{}_ptr", ident);
                let argc_ident = format_ident!("{}_argc", ident);
                quote! {
                    let #tmp: #rooted_slice = unsafe {
                        let __slice = ::std::slice::from_raw_parts(
                            #ptr_ident as *const #object,
                            #argc_ident as usize,
                        );
                        __gc_context.root_slice(__slice)
                    };
                    let #ident = &#tmp;
                }
            }
            ArgKind::ObjectRef(ty) => {
                if has_gc_context {
                    panic!(
                        "heap reference arguments in #[internal_fn] with GcContext must be rewritten to &Rooted"
                    );
                }
                if !arg.info.is_ref || arg.info.is_mut {
                    panic!("heap reference arguments in #[internal_fn] must be borrowed as &T");
                }
                quote! {
                    let #tmp = #object(#ident as u64);
                    let #ident: &#ty = __unwrap_or_nil!((&#tmp).try_into());
                }
            }
            ArgKind::Primitive(ty) => {
                if ty.to_string() == "Symbol" {
                    quote! {
                        let #tmp = #object(#ident as u64);
                        let #ident: #ty = __unwrap_or_nil!((&#tmp).try_into());
                    }
                } else {
                    quote! {
                        let #ident = #ident as #ty;
                    }
                }
            }
            ArgKind::GcContext => {
                context_binding = Some(if arg.info.is_mut {
                    quote! {
                        let #ident = &mut __gc_context;
                    }
                } else {
                    quote! {
                        let #ident = &__gc_context;
                    }
                });
                quote! {}
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
                quote! {}
            }
            ArgKind::ObjectRef(_) => {
                quote! {}
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

    if let Some(binding) = context_binding {
        init_args.push(binding);
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
            ArgKind::GcContext => {}
            ArgKind::Slice(_) | ArgKind::RootedSlice => {
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
