#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};
use syn::{Error, Type};

use crate::defun::{Function, ArgInfo, ArgKind, RetKind};

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let lisp_name = spec.name.unwrap_or_else(|| map_function_name(&subr_name));
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let def_func_name = format_ident!("__def_{}", &subr_name);
    let register_func_name = format_ident!("__register_{}", &subr_name);
    let rust_wrapper_name = format_ident!("__rust_wrapper_{}", &subr_name);

    let args = function.args;
    let arg_conversion = get_arg_conversion(&args);

    // Generate direct parameter passing for internal functions
    let c_params: Vec<TokenStream> = args.iter().enumerate().map(|(i, (ident, ty, arg_info))| {
        match &arg_info.kind {
            ArgKind::Env => quote! { env: i64 },
            _ => quote! { #ident: i64 },
        }
    }).collect();

    let c_param_idents: Vec<Ident> = args.iter().enumerate().map(|(i, (ident, _, arg_info))| {
        match &arg_info.kind {
            ArgKind::Env => format_ident!("env"),
            _ => ident.clone(),
        }
    }).collect();

    // Generate the actual function call
    let call_args = args.iter().map(|(ident, _, _)| quote! { #ident });

    let ret_is_unit = matches!(function.ret_kind, RetKind::Unit);
    let wrapper_ret_ty = if ret_is_unit {
        quote! { ::std::result::Result<(), &'static str> }
    } else {
        quote! { ::std::result::Result<i64, &'static str> }
    };

    let wrapper_result = match &function.ret_kind {
        RetKind::Unit => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(()) => Ok(()),
                        Err(_) => Err("call error"),
                    }
                }
            } else {
                quote! {
                    let _ = result;
                    Ok(())
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
                            let v: crate::core::value::Value = ::std::convert::Into::into(val);
                            Ok(v.0 as i64)
                        }
                        Err(_) => Err("call error"),
                    }
                }
            } else {
                quote! {
                    let v: crate::core::value::Value = ::std::convert::Into::into(result);
                    Ok(v.0 as i64)
                }
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
        RetKind::Slice(_) => {
            quote! {
                return Err("unsupported return type");
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

    let mut return_sigs: Vec<TokenStream> = Vec::new();
    if !ret_is_unit {
        return_sigs.push(quote! {
            sig.returns.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64));
        });
    }

    let extern_fn = if ret_is_unit {
        quote! {
            #[automatically_derived]
            #[doc(hidden)]
            unsafe extern "C" fn #func_name(#(#c_params),*) {
                let _ = #rust_wrapper_name(#(#c_param_idents),*);
            }
        }
    } else {
        quote! {
            #[automatically_derived]
            #[doc(hidden)]
            unsafe extern "C" fn #func_name(#(#c_params),*) -> i64 {
                match #rust_wrapper_name(#(#c_param_idents),*) {
                    Ok(v) => v,
                    Err(_) => crate::core::value::NIL as i64,
                }
            }
        }
    };

    // Generate direct parameter signatures for internal functions
    let signatures: Vec<TokenStream> = args.iter().map(|(_, _, arg_info)| {
        match &arg_info.kind {
            ArgKind::Env => quote! {
                sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // env
            },
            _ => quote! {
                sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64)); // param
            },
        }
    }).collect();

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

        inventory::submit!(crate::core::compiler::BuiltinFnPlugin::new(#def_func_name, #register_func_name, false));

        #[automatically_derived]
        #[doc(hidden)]
        #[inline(always)]
        unsafe fn #rust_wrapper_name(
            #(#c_params),*
        ) -> #wrapper_ret_ty {
            #(#arg_conversion)*
            let result = #subr(#(#call_args),*);
            #wrapper_result
        }

        #extern_fn

        #body
    }
}

fn get_arg_conversion(args: &[(Ident, Type, ArgInfo)]) -> Vec<TokenStream> {
    let mut conversions = Vec::new();
    
    // Convert each argument directly from i64 parameter
    for (i, (ident, ty, arg_info)) in args.iter().enumerate() {
        let conversion = match &arg_info.kind {
            ArgKind::Env => {
                quote! {
                    let #ident = env as *const crate::core::env::Environment;
                    let #ident = #ident.as_ref().ok_or("failed to convert env")?;
                }
            }
            ArgKind::Value => {
                if arg_info.is_ref {
                    let tmp = format_ident!("__arg_val_{}", i);
                    let mut_tok = if arg_info.is_mut { quote! { mut } } else { quote! {} };
                    let ref_tok = if arg_info.is_mut { quote! { &mut #tmp } } else { quote! { &#tmp } };
                    quote! {
                        let #mut_tok #tmp = crate::core::value::Value(#ident as u64);
                        let #ident = #ref_tok;
                    }
                } else {
                    quote! {
                        let #ident = crate::core::value::Value(#ident as u64);
                    }
                }
            }
            ArgKind::FromValue => {
                if arg_info.is_ref {
                    let tmp_val = format_ident!("__arg_val_{}", i);
                    let tmp_cast = format_ident!("__arg_cast_{}", i);
                    let mut_val = if arg_info.is_mut { quote! { mut } } else { quote! {} };
                    let ref_tok = if arg_info.is_mut { quote! { &mut #tmp_cast } } else { quote! { &#tmp_cast } };
                    quote! {
                        let #mut_val #tmp_val = crate::core::value::Value(#ident as u64);
                        let #mut_val #tmp_cast: #ty = ::std::convert::TryFrom::try_from(#tmp_val)?;
                        let #ident = #ref_tok;
                    }
                } else {
                    quote! {
                        let #ident: #ty = ::std::convert::TryFrom::try_from(crate::core::value::Value(#ident as u64))?;
                    }
                }
            }
            _ => {
                quote! {
                    let #ident: #ty = ::std::convert::TryFrom::try_from(#ident)?;
                }
            }
        };
        
        conversions.push(conversion);
    }
    
    conversions
}

fn map_function_name(name: &str) -> String {
    name.replace('_', "-")
}

#[derive(Default, PartialEq, Debug, FromMeta)]
pub(crate) struct Spec {
    #[darling(default)]
    name: Option<String>,
}
