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
    let rust_wrapper_name = format_ident!("__rust_wrapper_{}", &subr_name);
    // let name_lit = Literal::string(&runtime_fn.sig.ident.to_string());

    let args = function.args;
    let arg_conversion = get_arg_conversion(&args);


    // Generate the extern "C" function signature
    // let c_params = (0..args.len()).map(|i| quote! { arg_#i: i64 });
    let mut c_param_idents: Vec<Ident> = Vec::new();
    let mut c_params: Vec<TokenStream> = Vec::new();
    for (i, (_ident, _ty, arg_info)) in args.iter().enumerate() {
        match &arg_info.kind {
            ArgKind::Slice(_) => {
                let ptr = format_ident!("arg_{}_ptr", i);
                let len = format_ident!("arg_{}_len", i);
                c_params.push(quote! { #ptr: i64 });
                c_params.push(quote! { #len: i64 });
                c_param_idents.push(ptr);
                c_param_idents.push(len);
            }
            _ => {
                let id = format_ident!("arg_{}", i);
                c_params.push(quote! { #id: i64 });
                c_param_idents.push(id);
            }
        }
    }

    

    // Generate the actual function call
    let call_args = args.iter().map(|(ident, _, _)| quote! { #ident });
    
    let wrapper_result = if function.fallible {
        quote! {
            match result {
                Ok(val) => Ok(val.0 as i64),
                Err(_) => Err("call error"),
            }
        }
    } else {
        quote! {
            Ok(result.0 as i64)
        }
    };

    let mut signatures: Vec<TokenStream> = Vec::new();
    for (_, _, arg_info) in args.iter() {
        match &arg_info.kind {
            ArgKind::Slice(_) => {
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64));
                });
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64));
                });
            }
            _ => {
                signatures.push(quote! {
                    sig.params.push(cranelift::prelude::AbiParam::new(cranelift::prelude::codegen::ir::types::I64));
                });
            }
        }
    }

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

        inventory::submit!(crate::runtime::BuiltinFnPlugin::new(#def_func_name, #register_func_name));


        #[automatically_derived]
        #[doc(hidden)]
        fn #rust_wrapper_name(
            #(#c_params),*
        ) -> ::std::result::Result<i64, &'static str> {
            #(#arg_conversion)*
            let result = #subr(#(#call_args),*);
            #wrapper_result
        }

        #[automatically_derived]
        #[doc(hidden)]
        unsafe extern "C" fn #func_name(
            #(#c_params),*
        ) -> i64 {
            match #rust_wrapper_name(#(#c_param_idents),*) {
                Ok(v) => v,
                Err(_) => 0,
            }
        }

        #body
    }
}

fn param_name(i: usize) -> Ident {
    let param_name = format_ident!("arg_{}", i);
    param_name
}

fn get_arg_conversion(args: &[(Ident, Type, ArgInfo)]) -> Vec<TokenStream> {
    args.iter()
        .enumerate()
        .map(|(i, (ident, ty, arg_info))| {
            let param = param_name(i);

            match &arg_info.kind {
                ArgKind::Env => {
                    quote! {
                        let #ident = #param as *mut crate::core::env::Environment;
                    }
                }

                // Direct Value reconstruction (by bits)
                ArgKind::Value => {
                    if arg_info.is_ref {
                        let tmp = format_ident!("__arg_val_{}", i);
                        let mut_tok = if arg_info.is_mut { quote! { mut } } else { quote! {} };
                        let ref_tok = if arg_info.is_mut { quote! { &mut #tmp } } else { quote! { &#tmp } };
                        quote! {
                            let #mut_tok #tmp = crate::core::value::Value(#param as u64);
                            let #ident = #ref_tok;
                        }
                    } else {
                        quote! {
                            let #ident = crate::core::value::Value(#param as u64);
                        }
                    }
                }

                // Types that require TryFrom<Value>
                ArgKind::FromValue => {
                    if arg_info.is_ref {
                        let tmp_val = format_ident!("__arg_val_{}", i);
                        let tmp_cast = format_ident!("__arg_cast_{}", i);
                        let mut_val = if arg_info.is_mut { quote! { mut } } else { quote! {} };
                        let ref_tok = if arg_info.is_mut { quote! { &mut #tmp_cast } } else { quote! { &#tmp_cast } };
                        quote! {
                            let #mut_val #tmp_val = crate::core::value::Value(#param as u64);
                            let #tmp_cast: #ty = ::std::convert::TryFrom::try_from(#tmp_val)?;
                            let #ident = #ref_tok;
                        }
                    } else {
                        quote! {
                            let #ident: #ty = ::std::convert::TryFrom::try_from(crate::core::value::Value(#param as u64))?;
                        }
                    }
                }

                // Option<T> where T is Value-like or FromValue-like, with NIL as None
                ArgKind::Option(inner_kind) => {
                    let is_ref = arg_info.is_ref;
                    let is_mut = arg_info.is_mut;

                    match &**inner_kind {
                        // Option<Value> or Option<&Value>
                        ArgKind::Value => {
                            if is_ref {
                                let tmp = format_ident!("__arg_val_{}", i);
                                let mut_tok = if is_mut { quote! { mut } } else { quote! {} };
                                let ref_tok = if is_mut { quote! { &mut #tmp } } else { quote! { &#tmp } };
                                quote! {
                                    let #ident = if #param == crate::core::value::NIL {
                                        None
                                    } else {
                                        let #mut_tok #tmp = crate::core::value::Value(#param as u64);
                                        Some(#ref_tok)
                                    };
                                }
                            } else {
                                quote! {
                                    let #ident = if #param == crate::core::value::NIL {
                                        None
                                    } else {
                                        Some(crate::core::value::Value(#param as u64))
                                    };
                                }
                            }
                        }

                        // Option<T> or Option<&T> where T: TryFrom<Value>
                        ArgKind::FromValue => {
                            if is_ref {
                                let tmp_val = format_ident!("__arg_val_{}", i);
                                let tmp_cast = format_ident!("__arg_cast_{}", i);
                                let mut_val = if is_mut { quote! { mut } } else { quote! {} };
                                let ref_tok = if is_mut { quote! { &mut #tmp_cast } } else { quote! { &#tmp_cast } };
                                quote! {
                                    let #ident = if #param == crate::core::value::NIL {
                                        None
                                    } else {
                                        let #mut_val #tmp_val = crate::core::value::Value(#param as u64);
                                        let #tmp_cast: #ty = ::std::convert::TryFrom::try_from(#tmp_val)?;
                                        Some(#ref_tok)
                                    };
                                }
                            } else {
                                quote! {
                                    let #ident = if #param == crate::core::value::NIL {
                                        None
                                    } else {
                                        Some(::std::convert::TryFrom::try_from(crate::core::value::Value(#param as u64))?)
                                    };
                                }
                            }
                        }

                        // Fallback: transmute inside Some
                        _ => {
                            quote! {
                                let #ident = if #param == crate::core::value::NIL {
                                    None
                                } else {
                                    Some(unsafe { ::std::mem::transmute::<i64, #ty>(#param) })
                                };
                            }
                        }
                    }
                }

                // Slice arguments: reconstruct from (ptr,len)
                ArgKind::Slice(inner_kind) => {
                    // arg_i comes in as two C-ABI params: arg_{i}_ptr (i64) and arg_{i}_len (i64)
                    let ptr_param = format_ident!("arg_{}_ptr", i);
                    let len_param = format_ident!("arg_{}_len", i);
                    let vec_name = format_ident!("__arg_slice_vec_{}", i);
                    let len_name = format_ident!("__arg_slice_len_{}", i);
                    let ptr_cast = quote! { #ptr_param as *const i64 };

                    // Whether the Rust parameter is &mut [..]
                    let ref_tok = if arg_info.is_mut { quote! { &mut #vec_name[..] } } else { quote! { &#vec_name[..] } };

                    // If the inner item requires TryFrom<Value>
                    match &**inner_kind {
                        ArgKind::Value => {
                            quote! {
                                let #len_name: usize = ::std::convert::TryFrom::try_from(#len_param)
                                    .map_err(|_| "invalid slice length")?;
                                let __ptr = #ptr_cast;
                                let mut #vec_name: ::std::vec::Vec<crate::core::value::Value> = ::std::vec::Vec::with_capacity(#len_name);
                                for __i in 0..#len_name {
                                    let __raw = unsafe { __ptr.add(__i).read() as u64 };
                                    let __val = crate::core::value::Value::from_raw_inc_rc(__raw);
                                    #vec_name.push(__val);
                                }
                                let #ident = #ref_tok;
                            }
                        }
                        ArgKind::FromValue => {
                            // Determine the element type from the Rust type: it must be a slice [T]
                            let elem_ty: &syn::Type = match ty {
                                syn::Type::Slice(s) => s.elem.as_ref(),
                                _ => ty, // fallback; shouldn't happen
                            };
                            quote! {
                                let #len_name: usize = ::std::convert::TryFrom::try_from(#len_param)
                                    .map_err(|_| "invalid slice length")?;
                                let __ptr = #ptr_cast;
                                let mut #vec_name: ::std::vec::Vec<#elem_ty> = ::std::vec::Vec::with_capacity(#len_name);
                                for __i in 0..#len_name {
                                    let __raw = unsafe { __ptr.add(__i).read() as u64 };
                                    let __val = crate::core::value::Value::from_raw_inc_rc(__raw);
                                    let __cast: #elem_ty = ::std::convert::TryFrom::try_from(__val)?;
                                    #vec_name.push(__cast);
                                }
                                let #ident = #ref_tok;
                            }
                        }
                        _ => {
                            // Fallback: treat as slice of raw i64 payloads
                            quote! {
                                let #len_name: usize = ::std::convert::TryFrom::try_from(#len_param)
                                    .map_err(|_| "invalid slice length")?;
                                let __ptr = #ptr_cast;
                                // Rebuild a Vec<i64> and borrow it as a slice
                                let mut #vec_name: ::std::vec::Vec<i64> = ::std::vec::Vec::with_capacity(#len_name);
                                for __i in 0..#len_name {
                                    let __raw = unsafe { __ptr.add(__i).read() };
                                    #vec_name.push(__raw);
                                }
                                let #ident = #ref_tok;
                            }
                        }
                    }
                }

                // Fallback for other ABI-passed primitives: transmute raw i64
                ArgKind::Other => {
                    quote! {
                        let #ident = unsafe { ::std::mem::transmute::<i64, #ty>(#param) };
                    }
                }
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


struct ArgInfo {
    kind: ArgKind,
    is_mut: bool,
    is_ref: bool,
}

#[derive(PartialEq, Debug, Clone)]
enum ArgKind {
    Env,
    Value,
    FromValue,
    Option(Box<ArgKind>),
    Slice(Box<ArgKind>),
    Other,
}

pub(crate) struct Function {
    name: syn::Ident,
    body: syn::Item,
    args: Vec<(syn::Ident, syn::Type, ArgInfo)>,
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


fn parse_signature(sig: &syn::Signature) -> Result<Vec<(syn::Ident, syn::Type, ArgInfo)>, Error> {
    let mut args = Vec::new();
    for input in &sig.inputs {
        match input {
            syn::FnArg::Receiver(x) => {
                return Err(Error::new_spanned(x, "Self is not valid in lisp functions"));
            }
            syn::FnArg::Typed(pat_type) => {
                let ty = pat_type.ty.as_ref().clone();
                let (arg_info,  ty) = get_arg_type(&ty)?;
                
                // Extract the identifier from the pattern
                let ident = match pat_type.pat.as_ref() {
                    syn::Pat::Ident(pat_ident) => pat_ident.ident.clone(),
                    _ => return Err(Error::new_spanned(pat_type, "Only simple identifiers are supported as function arguments")),
                };
                
                args.push((ident, ty.to_owned(), arg_info));
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

fn get_arg_type(ty: &syn::Type) -> Result<(ArgInfo, &syn::Type), Error> {
    let (mut inner_ty, mut is_ref, mut is_mut) = match ty {
        syn::Type::Reference(syn::TypeReference {
            elem, mutability, ..
        }) => (elem.as_ref(), true, mutability.is_some()),
        _ => (ty, false, false),
    };

    let kind = match inner_ty {
        syn::Type::Path(path) => {
            let name = get_path_ident_name(path);
            match &*name {
                "Option" => {
                    let outer = path.path.segments.last().unwrap();
                    let option_ty = get_generic_param(outer).expect("incorrect Option types");
                    let (info, ty) = get_arg_type(option_ty)?;
                    inner_ty = ty; // ensure we return the inner T for Option<T>
                    // TODO we are not expecting double reference, so this is fine
                    is_ref = is_ref | info.is_ref;
                    is_mut = is_mut | info.is_mut;
                    ArgKind::Option(Box::new(info.kind))
                }
                _ => get_object_kind(path),
            }
        }
        syn::Type::Slice(slice) => {
            // Slice element kind
            let (elem_info, _elem_ty) = get_arg_type(&slice.elem)?;
            // Slices must be passed by reference at the Rust boundary.
            is_ref = true | is_ref;
            ArgKind::Slice(Box::new(elem_info.kind))
        }
        _ => ArgKind::Other,
    };

    Ok((ArgInfo { kind, is_mut, is_ref }, inner_ty))
}

fn get_object_kind(type_path: &syn::TypePath) -> ArgKind {
    let outer_type = type_path.path.segments.last().unwrap();
    if outer_type.ident == "Value" {
        ArgKind::Value
    } else if outer_type.ident == "LispString"
        || outer_type.ident == "Symbol"
        || outer_type.ident == "Vector"
        || outer_type.ident == "Cons"
        || outer_type.ident == "Function"
        || outer_type.ident == "i64"
    {
        ArgKind::FromValue
    } else if outer_type.ident == "Environment" {
        ArgKind::Env
    } else {
        ArgKind::Other
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
