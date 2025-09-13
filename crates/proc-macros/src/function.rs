use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{Error, Ident};

pub(crate) struct Arg {
    pub(crate) info: ArgInfo,
    pub(crate) ident: Ident,
}

pub(crate) struct ArgInfo {
    pub(crate) kind: ArgKind,
    pub(crate) is_mut: bool,
    // pub(crate) ident: Ident,
    pub(crate) is_ref: bool,
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum ArgKind {
    Env,
    /// an Object. can be referenced, but should not be mut refed.
    /// don't accept LispObject for now, because the methods for object is convenient enough.
    Object,
    /// i64, f64, etc. This will behave differently between defun and internal_fn.
    /// This is converted to Number types in defun, and is loaded directly in internal_fn.
    Primitive(Ident),
    /// things like LispFunction or LispVector, the data inside GC pointer. this type
    /// must be referenced.
    ObjectRef(Ident),
    /// &optional equivalent.
    Option(Box<ArgKind>),
    /// &rest equivallent.
    Slice(Box<ArgKind>),
    /// internal_fn used only.
    Other(syn::Type),
}

impl ArgKind {
    fn is_other(&self) -> bool {
        match self {
            ArgKind::Other(_) => true,
            ArgKind::Slice(inner) => inner.is_other(),
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum RetKind {
    Unit,                 // no return
    Primitive(String),    // i64, u64, etc.
    Object,               // crate::core::object::Object
    Option(Box<RetKind>), // Option<...> where inner is Object
    Other,
}

pub(crate) struct Function {
    pub(crate) name: syn::Ident,
    pub(crate) body: syn::Item,
    pub(crate) args: Vec<Arg>,
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
                let is_lisp_subr = args.iter().all(|arg| !arg.info.kind.is_other());
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

fn parse_signature(sig: &syn::Signature) -> Result<Vec<Arg>, Error> {
    let mut args = Vec::new();
    for input in &sig.inputs {
        match input {
            syn::FnArg::Receiver(x) => {
                return Err(Error::new_spanned(x, "Self is not valid in lisp functions"));
            }
            syn::FnArg::Typed(pat_type) => {
                let ty = pat_type.ty.as_ref();
                let arg_info = get_arg_type(ty)?;

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

                args.push(Arg {
                    info: arg_info,
                    ident,
                });
            }
        }
    }
    Ok(args)
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
        syn::Type::Path(path) => {
            let name = get_path_ident_name(path);
            match &*name {
                "Option" => {
                    let outer = path.path.segments.last().unwrap();
                    let inner_ty = get_generic_param(outer).expect("Option must have inner type");
                    let inner = classify_return_type(inner_ty);
                    RetKind::Option(Box::new(inner))
                }
                // Exact Object-like
                n if is_object_name(n) => RetKind::Object,
                n if is_primitive_name(n) => RetKind::Primitive(n.to_string()),
                // Convertible into Object (align with get_object_kind FromValue set)
                _ => RetKind::Other,
            }
        }
        _ => RetKind::Other,
    }
}

fn get_arg_type(ty: &syn::Type) -> Result<ArgInfo, Error> {
    let (inner_ty, mut is_ref, mut is_mut) = match ty {
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
                    let info = get_arg_type(option_ty)?;
                    is_ref = is_ref | info.is_ref;
                    is_mut = is_mut | info.is_mut;
                    ArgKind::Option(Box::new(info.kind))
                }
                _ => get_object_kind(path).unwrap_or(ArgKind::Other(inner_ty.to_owned())),
            }
        }
        syn::Type::Slice(slice) => {
            // Slice element kind
            let elem_info = get_arg_type(&slice.elem)?;
            // Slices must be passed by reference at the Rust boundary.
            // is_ref = true | is_ref;
            ArgKind::Slice(Box::new(elem_info.kind))
        }
        _ => ArgKind::Other(inner_ty.to_owned()),
    };

    Ok(ArgInfo {
        kind,
        is_mut,
        is_ref,
    })
}

fn is_object_name(ident: &str) -> bool {
    match ident {
        "Object" => true,
        _ => false,
    }
}

// TODO move types to another crate so we can reuse function there
fn is_object_ref_name(ident: &str) -> bool {
    match ident {
        "String" | "Cons" | "Function" | "HashTable" | "Integer" | "Float"
        | "Character" | "Vector" => true,
        _ => false,
    }
}

fn is_lisp_object_name(ident: &str) -> bool {
    ident.strip_prefix("Lisp").is_some_and(is_object_ref_name)
}

fn is_primitive_name(ident: &str) -> bool {
    match ident {
        "Symbol" | "i64" | "u64" | "usize" | "u32" => true,
        _ => false,
    }
}

fn get_object_kind(type_path: &syn::TypePath) -> Option<ArgKind> {
    let outer_type = type_path.path.segments.last().unwrap();
    let ident = outer_type.ident.to_string();
    if is_object_name(&ident) {
        Some(ArgKind::Object)
    } else if is_object_ref_name(&ident) {
        Some(ArgKind::ObjectRef(outer_type.ident.to_owned()))
    } else if is_primitive_name(&ident) {
        Some(ArgKind::Primitive(outer_type.ident.to_owned()))
    } else if outer_type.ident == "Environment" {
        Some(ArgKind::Env)
    } else {
        None
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

fn get_path_ident_name(type_path: &syn::TypePath) -> String {
    type_path.path.segments.last().unwrap().ident.to_string()
}

pub fn construct_return(
    fallible: bool,
    success: TokenStream,
    fallback: TokenStream,
) -> TokenStream {
    if fallible {
        quote! {
            match result {
                Ok(val) => {
                    #success
                },
                Err(e) => {
                    tracing::error!("error: {e:?}");
                    Err("call error")
                }
            }
        }
    } else {
        quote! {
            #fallback
        }
    }
}

pub fn construct_objectref(is_mut: bool, cnt: usize, ident: &Ident, ty: &Ident) -> TokenStream {
    let mut_val = if is_mut {
        quote! {mut}
    } else {
        quote! {}
    };
    let object = quote! { crate::core::object::Object };
    let tmp = format_ident!("__arg_val_{}", cnt);
    quote! {
        let #tmp = #object(#ident as u64);
        let #ident: &#mut_val #ty = (&#tmp).try_into()?;
    }
}

pub fn construct_return_nodrop(val: &str) -> TokenStream {
    let val = format_ident!("{val}");
    quote! {
        let result = Ok(#val.0 as i64);
        std::mem::forget(#val);
        result
    }
}
