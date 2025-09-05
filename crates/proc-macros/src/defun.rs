#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::{Literal, TokenStream, Ident};
use quote::{format_ident, quote};
use syn::{Error, Type};

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {
    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let lisp_name = spec.name.unwrap_or_else(|| map_function_name(&subr_name));
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let def_func_name = format_ident!("__def_{}", &subr_name);
    let register_func_name = format_ident!("__register_{}", &subr_name);
    // let name_lit = Literal::string(&runtime_fn.sig.ident.to_string());

    let args = function.args;
    let arg_conversion = get_arg_conversion(&args);

    let err = if function.fallible {
        quote! {?}
    } else {
        quote! {}
    };

    // Generate the extern "C" function signature
    // let c_params = (0..args.len()).map(|i| quote! { arg_#i: i64 });
    let c_params = (0..args.len()).map(|i| format_ident!("arg_{}", i)).map(|i| quote! {#i: i64});

    
    // Generate conversions from i64 to actual types
    let conversions = args.iter().enumerate().map(|(i, (ident, ty, arg_type))| {
        let param_name = param_name(i);
        match arg_type {
            ArgType::Env(_) => quote! {
                let #ident = #param_name as *mut crate::core::env::Envrionment;
            },
            ArgType::Gc => quote! {
                let #ident = unsafe {
                    let val = crate::core::value::Value(#param_name as u64);
                    crate::core::gc::Gc::try_from(val).unwrap()
                };
            },
            ArgType::Value => quote! {
                let #ident = crate::core::value::Value(#param_name as u64);
            },
            ArgType::IntoValue => quote! {
                let #ident = crate::core::value::Value(#param_name as u64);
            },
            ArgType::Option => quote! {
                let #ident = if #param_name == crate::core::value::NIL {
                    None
                } else {
                    Some(crate::core::value::Value(#param_name as u64))
                };
            },
            ArgType::Other => quote! {
                let #ident = std::mem::transmute::<i64, #ty>(#param_name);
            },
        }
    });

    // Generate the actual function call
    let call_args = args.iter().map(|(ident, _, _)| quote! { #ident });
    
    // Handle result conversion
    let result_conversion = if function.fallible {
        quote! {
            match result {
                Ok(val) => val.0 as i64,
                Err(_) => 0, // Handle error appropriately
            }
        }
    } else {
        quote! {
            result.0 as i64
        }
    };

    let signatures = args.iter().map(|_| quote! {
        sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64));
    });

    quote! {

        #[automatically_derived]
        #[doc(hidden)]
        fn #def_func_name<T: cranelift_module::Module>(module: &mut T) -> anyhow::Result<cranelift_module::FuncId> {
            let mut sig = module.make_signature();
            #(#signatures)*
            sig.returns.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64));

            let func_id = module.declare_function(#lisp_name, cranelift_module::Linkage::Import, &sig)?;
            Ok(func_id)

        }

        #[automatically_derived]
        #[doc(hidden)]
        fn #register_func_name(jit_builder: &mut cranelift_jit::JITBuilder) {

            jit_builder.symbol(#lisp_name, #func_name as *const u8);

        }

        #[allow_unused]
        inventory::submit!(crate::runtime::BuiltinFnPlugin::new(#def_func_name, #register_func_name));


        #[automatically_derived]
        #[doc(hidden)]
        unsafe extern "C" fn #func_name(
            #(#c_params),*
        ) -> i64 {
            #(#conversions)*
            
            let result = #subr(#(#call_args),*)#err;
            
            #result_conversion
        }

        #body
    }
}

fn param_name(i: usize) -> Ident {
    let param_name = format_ident!("arg_{}", i);
    param_name
}

fn get_arg_conversion(args: &[(Ident, Type, ArgType)]) -> Vec<TokenStream> {
    args.iter()
        .enumerate()
        .map(|(idx, (ident, ty, arg_type))| {
            let param_name = param_name(idx);
            match arg_type {
            ArgType::Env(_) => quote! { env as i64 },
            ArgType::Gc => {
                quote! { 
                    {
                        let val: crate::core::value::Value = args.get(#idx);
                        val.0 as i64
                    }
                }
            }
            ArgType::Option => {
                quote! {
                    {
                        match args.get(#idx) {
                            Some(x) => {
                                let val: crate::core::value::Value = *x;
                                val.0 as i64
                            },
                            None => 0i64,
                        }
                    }
                }
            }
            ArgType::Value => {
                quote! { 
                    {
                        let val: crate::core::value::Value = args.get(#idx);
                        val.0 as i64
                    }
                }
            }
            ArgType::IntoValue => {
                quote! {
                    {
                        let val: crate::core::value::Value = args.get(#idx).into();
                        val.0 as i64
                    }
                }
            }
            ArgType::Other => {
                quote! {
                    {
                        let val: i64 = std::convert::TryFrom::try_from(&args[#idx])?;
                        val
                    }
                }
            }
            }
            }
        )
        .collect()
}


fn get_path_ident_name(type_path: &syn::TypePath) -> String {
    type_path.path.segments.last().unwrap().ident.to_string()
}

fn map_function_name(name: &str) -> String {
    name.replace('_', "-")
}


struct ArgTy {
    ty: ArgType,
    is_mut: bool,
    is_ref: bool,
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum ArgType {
    Env(bool),
    Gc,
    // Slice(Gc),
    // SliceRt(Gc),
    // ArgSlice,
    Value,
    IntoValue,
    Option,
    // OptionRt,
    Other,
}

pub(crate) struct Function {
    name: syn::Ident,
    body: syn::Item,
    args: Vec<(syn::Ident, syn::Type, ArgType)>,
    fallible: bool,
}

impl syn::parse::Parse for Function {
    fn parse(input: syn::parse::ParseStream) -> Result<Self, Error> {
        let item: syn::Item = input.parse()?;
        parse_fn(item)
    }
}

fn parse_fn(item: syn::Item) -> Result<Function, Error> {
    match item {
        syn::Item::Fn(syn::ItemFn { ref sig, .. }) => {
            if sig.unsafety.is_some() {
                Err(Error::new_spanned(sig, "lisp functions cannot be `unsafe`"))
            } else {
                let args = parse_signature(sig)?;
                let fallible = return_type_is_result(&sig.output);
                Ok(Function {
                    name: sig.ident.clone(),
                    body: item,
                    args,
                    fallible,
                })
            }
        }
        _ => Err(Error::new_spanned(
            item,
            "`lisp_fn` attribute can only be used on functions",
        )),
    }
}


fn parse_signature(sig: &syn::Signature) -> Result<Vec<(syn::Ident, syn::Type, ArgType)>, Error> {
    let mut args = Vec::new();
    for input in &sig.inputs {
        match input {
            syn::FnArg::Receiver(x) => {
                return Err(Error::new_spanned(x, "Self is not valid in lisp functions"));
            }
            syn::FnArg::Typed(pat_type) => {
                let ty = pat_type.ty.as_ref().clone();
                let arg_type = get_arg_type(&ty)?;
                
                // Extract the identifier from the pattern
                let ident = match pat_type.pat.as_ref() {
                    syn::Pat::Ident(pat_ident) => pat_ident.ident.clone(),
                    _ => return Err(Error::new_spanned(pat_type, "Only simple identifiers are supported as function arguments")),
                };
                
                args.push((ident, ty, arg_type));
            }
        }
    }
    Ok(args)
}

fn return_type_is_result(output: &syn::ReturnType) -> bool {
    match output {
        syn::ReturnType::Type(_, ty) => match ty.as_ref() {
            syn::Type::Path(path) => get_path_ident_name(path) == "Result",
            _ => false,
        },
        syn::ReturnType::Default => false,
    }
}

fn get_arg_type(ty: &syn::Type) -> Result<ArgType, Error> {
    let is_ref = matches!(ty, syn::Type::Reference(x));
    Ok(match ty {
        syn::Type::Reference(syn::TypeReference {
            elem, mutability, ..
        }) => match elem.as_ref() {
            syn::Type::Path(path) => match &*get_path_ident_name(path) {
                "Envrionment" => ArgType::Env(mutability.is_some()),
                // "Rt" | "Rto" => get_rt_type(path, mutability.is_some())?,
                _ => ArgType::Other,
            },
            ty @ _ => get_arg_type(ty)?,
        },
        syn::Type::Path(path) => {
            let name = get_path_ident_name(path);
            match &*name {
                "Option" => {
                    let outer = path.path.segments.last().unwrap();
                    match get_generic_param(outer) {
                        Some(syn::Type::Reference(inner)) => match inner.elem.as_ref() {
                            syn::Type::Path(path) => match &*get_path_ident_name(path) {
                                _ => ArgType::Option,
                            },
                            _ => ArgType::Option,
                        },
                        _ => ArgType::Option,
                    }
                }
                "OptionalFlag" => ArgType::Option,
                _ => get_object_type(path),
            }
        }
        _ => ArgType::Other,
    })
}

fn get_object_type(type_path: &syn::TypePath) -> ArgType {
    let outer_type = type_path.path.segments.last().unwrap();
    if outer_type.ident == "Value" {
        ArgType::Value
    } else if outer_type.ident == "LispString"
        || outer_type.ident == "Symbol"
        || outer_type.ident == "Vector"
        || outer_type.ident == "Cons"
        || outer_type.ident == "Function"
        || outer_type.ident == "i64"
    {
        ArgType::IntoValue
    } else if outer_type.ident == "Gc" {
        ArgType::Gc
    } else if outer_type.ident == "Environment" {
        ArgType::Env(false)
    } else {
        ArgType::Other
    }
}

fn get_generic_param(outer_type: &syn::PathSegment) -> Option<&syn::Type> {
    match &outer_type.arguments {
        syn::PathArguments::AngleBracketed(generic) => match generic.args.first().unwrap() {
            syn::GenericArgument::Type(ty) => Some(ty),
            _ => None,
        },
        _ => None,
    }
}

#[derive(Default, PartialEq, Debug, FromMeta)]
pub(crate) struct Spec {
    #[darling(default)]
    name: Option<String>,
    #[darling(default)]
    required: Option<u16>,
}
