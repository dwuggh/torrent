#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::Error;

pub(crate) fn expand(function: Function, spec: Spec) -> TokenStream {

    let body = function.body;
    let subr = function.name;
    let subr_name = subr.to_string();
    let func_name = format_ident!("__wrapper_fn_{}", &subr_name);
    let lisp_name = spec.name.unwrap_or_else(|| map_function_name(&subr_name));

    let args = function.args;
    let arg_conversion = get_arg_conversion(&args);

    let err = if function.fallible {
        quote! {?}
    } else {
        quote! {}
    };

    // Create the context from a pointer to get around the issue that the
    // return val is bound to the mutable borrow, meaning we can use them
    // both in the into_obj function. Similar to the rebind! macro.
    let subr_call = quote! {
        let ptr = cx as *mut crate::core::gc::Context;
        let val = #subr(#(#arg_conversion),*)#err;
        let cx: &'ob mut crate::core::gc::Context = unsafe {&mut *ptr};
        Ok(crate::core::object::IntoObject::into_obj(val, cx).into())
    };

    quote! {
        #[automatically_derived]
        #[doc(hidden)]
        fn #func_name<'ob>(
            #(#arg_conversion),*
        ) -> anyhow::Result<Value> {
            #subr_call
        }

        #body
    }
}

fn get_arg_conversion(args: &[ArgType]) -> Vec<TokenStream> {
    args.iter()
        .enumerate()
        .map(|(idx, arg_type)| match arg_type {
            ArgType::Env(_) => quote! {env},
            ArgType::Gc => {
                quote! { args.get(#idx): i64 }
                    }
            ArgType::Option => {
                        let bind = quote! {x.bind(cx)};
                        quote! {
                            match args.get(#idx) {
                                Some(x) => crate::core::object::Gc::try_from_option(#bind)?,
                                None => None,
                            }
                        }
            }
            ArgType::Value => {
                quote! { args.get(#idx): i64 }
            }
            ArgType::IntoValue => quote! { val.into() },
            ArgType::Other => {
                        quote! { std::convert::TryFrom::try_from(&args[#idx])? }
                    }
        })
        .collect()
}


fn get_path_ident_name(type_path: &syn::TypePath) -> String {
    type_path.path.segments.last().unwrap().ident.to_string()
}

fn map_function_name(name: &str) -> String {
    name.replace('_', "-")
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

impl ArgType {
    fn is_required_arg(self) -> bool {
        use ArgType as A;
        matches!(self, A::Value | A::IntoValue | A::Other)
    }

    fn is_positional_arg(self) -> bool {
        use ArgType as A;
        matches!(
            self,
            A::IntoValue | A::Value | A::Other | A::Option
        )
    }

    fn is_rest_arg(self) -> bool {
        false
    }
}

pub(crate) struct Function {
    name: syn::Ident,
    body: syn::Item,
    args: Vec<ArgType>,
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


fn parse_signature(sig: &syn::Signature) -> Result<Vec<ArgType>, Error> {
    let mut args = Vec::new();
    for input in &sig.inputs {
        match input {
            syn::FnArg::Receiver(x) => {
                return Err(Error::new_spanned(x, "Self is not valid in lisp functions"));
            }
            syn::FnArg::Typed(pat_type) => {
                let ty = pat_type.ty.as_ref().clone();
                let arg = get_arg_type(&ty)?;
                args.push(arg);
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
    Ok(match ty {
        syn::Type::Reference(syn::TypeReference {
            elem, mutability, ..
        }) => match elem.as_ref() {
            syn::Type::Path(path) => match &*get_path_ident_name(path) {
                // "Context" => ArgType::Context(mutability.is_some()),
                // "Rt" | "Rto" => get_rt_type(path, mutability.is_some())?,
                _ => ArgType::Other,
            },
            _ => ArgType::Other,
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
