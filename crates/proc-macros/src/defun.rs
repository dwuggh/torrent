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
            let args_cnt_u: usize = args_cnt as usize;
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
    let mut conversions = Vec::new();

    // Find the minimum required arguments (non-optional, non-slice)
    let mut required_args: usize = 0;
    let mut has_slice = false;
    let mut slice_start_idx = None;

    for (i, (_, _, arg_info)) in args.iter().enumerate() {
        match &arg_info.kind {
            ArgKind::Slice(_) => {
                has_slice = true;
                slice_start_idx = Some(i);
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
    for (i, (ident, ty, arg_info)) in args.iter().enumerate() {
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
                            let #ident = unsafe {
                                let ptr = args_ptr as *const i64;
                                let slice_ptr = ptr.add(#i);
                                std::slice::from_raw_parts(slice_ptr as *const crate::core::value::Value, slice_len)
                            };
                        }
                    }
                    ArgKind::FromValue => {
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
                    _ => {
                        quote! {
                            let slice_len = args_cnt_u - #i;
                            let mut slice_vec = Vec::with_capacity(slice_len);
                            unsafe {
                                let ptr = args_ptr as *const i64;
                                for j in 0..slice_len {
                                    let arg_val = ptr.add(#i + j).read();
                                    let converted: #ty = ::std::convert::TryFrom::try_from(arg_val)?;
                                    slice_vec.push(converted);
                                }
                            }
                            let #ident = slice_vec.as_slice();
                        }
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
                    ArgKind::FromValue => {
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
                        quote! {
                            let #ident = if args_cnt_u > #i {
                                let arg_val = unsafe {
                                    let ptr = args_ptr as *const i64;
                                    ptr.add(#i).read()
                                };
                                let converted: #ty = ::std::convert::TryFrom::try_from(arg_val)?;
                                Some(converted)
                            } else {
                                None
                            };
                        }
                    }
                };
                option_conversion
            }
            ArgKind::Value => {
                let load_arg = quote! {
                    let arg_val = unsafe {
                        let ptr = args_ptr as *const i64;
                        ptr.add(#i).read()
                    };
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
            ArgKind::FromValue => {
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
                let load_arg = quote! {
                    let arg_val = unsafe {
                        let ptr = args_ptr as *const i64;
                        ptr.add(#i).read()
                    };
                };
                quote! {
                    #load_arg
                    let #ident: #ty = ::std::convert::TryFrom::try_from(arg_val)?;
                }
            }
        };

        conversions.push(conversion);
    }

    conversions
}

fn get_path_ident_name(type_path: &syn::TypePath) -> String {
    type_path.path.segments.last().unwrap().ident.to_string()
}

fn map_function_name(name: &str) -> String {
    name.replace('_', "-")
}

pub(crate) struct ArgInfo {
    pub(crate) kind: ArgKind,
    pub(crate) is_mut: bool,
    pub(crate) is_ref: bool,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum ArgKind {
    Env,
    Value,
    FromValue,
    Option(Box<ArgKind>),
    Slice(Box<ArgKind>),
    Other,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum RetKind {
    Unit,                 // no return
    Value,                // crate::core::value::Value
    IntoValue,            // types convertible into Value
    Option(Box<RetKind>), // Option<...> where inner is Value or IntoValue
    Slice(Box<RetKind>),  // &[T] or &mut [T]
    Other,
}

pub(crate) struct Function {
    pub(crate) name: syn::Ident,
    pub(crate) body: syn::Item,
    pub(crate) args: Vec<(syn::Ident, syn::Type, ArgInfo)>,
    pub(crate) fallible: bool,
    pub(crate) is_lisp_subr: bool,
    pub(crate) ret_kind: RetKind,
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
                _ => RetKind::Other,
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
