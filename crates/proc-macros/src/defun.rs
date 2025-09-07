#![allow(clippy::manual_unwrap_or_default)]
use darling::FromMeta;
use proc_macro2::{Ident, Literal, TokenStream};
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
                    // TODO check if this is correct
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
        // Slice returns are recognized but not yet supported end-to-end here.
        // They will require two return values in the Cranelift signature and a wrapper shape to match.
        // For now, treat as unsupported to avoid silent ABI mismatches.
        RetKind::Slice(_) => {
            quote! {
                // You tried to export a function with an unsupported return type.
                // Please implement slice return ABI or change the return type.
                return Err("unsupported return type");
            }
        }
        RetKind::Other => {
            if function.fallible {
                quote! {
                    match result {
                        Ok(val) => Ok(val as *const _ as i64),
                        Err(_) => Err("call error"),
                    }
                }
            } else {
                quote! { Ok(result as *const _ as i64) }
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
                    Err(_) => 0,
                }
            }
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

    let is_lisp_subr = spec.is_lisp_subr && function.is_lisp_subr;
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

        inventory::submit!(crate::core::compiler::BuiltinFnPlugin::new(#def_func_name, #register_func_name, #is_lisp_subr));


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
                        let #ident = #ident.as_mut().ok_or("failed to convert env")?;
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
                            let #mut_val #tmp_cast: #ty = ::std::convert::TryFrom::try_from(#tmp_val)?;
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

                // Fallback for other ABI-passed primitives: transmute raw i64 for refs, try_from for non-refs
                ArgKind::Other => {
                    if arg_info.is_ref {
                        quote! {
                            let #ident = unsafe { ::std::mem::transmute::<i64, #ty>(#param) };
                        }
                    } else {
                        quote! {
                            let #ident: #ty = ::std::convert::TryFrom::try_from(#param)?;
                        }
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

#[derive(PartialEq, Debug, Clone)]
enum RetKind {
    Unit,                 // no return
    Value,                // crate::core::value::Value
    IntoValue,            // types convertible into Value
    Option(Box<RetKind>), // Option<...> where inner is Value or IntoValue
    Slice(Box<RetKind>),  // &[T] or &mut [T]
    Other,
}

pub(crate) struct Function {
    name: syn::Ident,
    body: syn::Item,
    args: Vec<(syn::Ident, syn::Type, ArgInfo)>,
    fallible: bool,
    is_lisp_subr: bool,
    ret_kind: RetKind,
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
                let is_lisp_subr = args.iter().all(|(_, _, info)| info.kind != ArgKind::Other);
                let (fallible, ret_kind) = return_type_info(&sig.output);
                Ok(Function {
                    name: sig.ident.clone(),
                    body: item,
                    args,
                    fallible,
                    is_lisp_subr,
                    ret_kind,
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
                let (arg_info, ty) = get_arg_type(&ty)?;

                // Extract the identifier from the pattern
                let ident = match pat_type.pat.as_ref() {
                    syn::Pat::Ident(pat_ident) => pat_ident.ident.clone(),
                    _ => {
                        return Err(Error::new_spanned(
                            pat_type,
                            "Only simple identifiers are supported as function arguments",
                        ));
                    }
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

fn return_type_info(output: &syn::ReturnType) -> (bool, RetKind) {
    match output {
        syn::ReturnType::Default => (false, RetKind::Unit),
        syn::ReturnType::Type(_, ty) => match ty.as_ref() {
            syn::Type::Tuple(t) if t.elems.is_empty() => (false, RetKind::Unit),
            syn::Type::Path(path) => {
                let name = get_path_ident_name(path);
                if name == "Result" {
                    let outer = path.path.segments.last().unwrap();
                    let ok_ty = get_generic_param(outer).expect("Result must have Ok type");
                    (true, classify_return_type(ok_ty))
                } else {
                    (false, classify_return_type(ty))
                }
            }
            _ => (false, classify_return_type(ty)),
        },
    }
}

fn classify_return_type(ty: &syn::Type) -> RetKind {
    match ty {
        syn::Type::Tuple(t) if t.elems.is_empty() => RetKind::Unit,
        syn::Type::Reference(r) => classify_return_type(r.elem.as_ref()),
        syn::Type::Slice(s) => {
            let inner = classify_return_type(s.elem.as_ref());
            RetKind::Slice(Box::new(inner))
        }
        syn::Type::Path(path) => {
            let name = get_path_ident_name(path);
            match &*name {
                "Option" => {
                    let outer = path.path.segments.last().unwrap();
                    let inner_ty = get_generic_param(outer).expect("Option must have inner type");
                    let inner = classify_return_type(inner_ty);
                    RetKind::Option(Box::new(inner))
                }
                // Exact Value-like
                "Value" | "RuntimeValue" => RetKind::Value,
                // Convertible into Value (align with get_object_kind FromValue set)
                "LispString" | "Symbol" | "Vector" | "Cons" | "Map" | "Function" | "i64" => {
                    RetKind::IntoValue
                }
                // Fallback to requiring Into<Value> at compile-time
                _ => RetKind::IntoValue,
            }
        }
        _ => RetKind::Other,
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

    Ok((
        ArgInfo {
            kind,
            is_mut,
            is_ref,
        },
        inner_ty,
    ))
}

fn get_object_kind(type_path: &syn::TypePath) -> ArgKind {
    let outer_type = type_path.path.segments.last().unwrap();
    if outer_type.ident == "Value" || outer_type.ident == "RuntimeValue" {
        ArgKind::Value
    } else if outer_type.ident == "LispString"
        || outer_type.ident == "Symbol"
        || outer_type.ident == "Vector"
        || outer_type.ident == "Cons"
        || outer_type.ident == "Function"
        || outer_type.ident == "Map"
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
    #[darling(default = "tru")]
    is_lisp_subr: bool,
}

fn tru() -> bool {
    true
}