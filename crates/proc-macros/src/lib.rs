use darling::{Error as DError, FromMeta, ast::NestedMeta};
use proc_macro::{self, TokenStream};
use proc_macro2::{Literal, Span};
use quote::{format_ident, quote};
use syn::{
    DataEnum, DataStruct, DeriveInput, Error, Fields, FnArg, GenericParam, Generics, Ident, ItemFn,
    LitStr, Member, Pat, PatIdent, PatType, Token, Type, TypePath, TypeReference, Visibility,
    parse_macro_input, punctuated::Punctuated,
};

mod defun;
mod variantly;

/// ## `#[defun]`
///
/// Represents the functions that are going to be hydrated to emacs lisp, through the `rune` VM execution. As of
/// today, they are not documented with the GNU Emacs documentation, though definitely is a point of improvement.
/// Following Rust convention, `defun` names are written in `snake_case`, though if you search them in GNU Emacs,
/// you'll find them in `kebab-case`.
///
/// Arguments of the `defun`s follow the arguments on its corresponding documentation, or rather, definition on
/// the C Emacs core.
///
/// ### Examples
///
/// For the `make-vector` function, here's its signature:
///
/// > make-vector is a function defined in `alloc.c`. Signature `(make-vector LENGTH INIT)`
///
/// Its corresponding `rune` `#[defun]` signature would be:
///
/// ```ignore
/// #[defun]
/// fn make_vector(length: usize, init: GcObj) -> Vec<GcObj> {}
/// ```
///
/// The return object is interesting, as it's not so easily inferrable from the signature, but rather from documentation.
/// In this case, the `make-vector` defun returns a *newly created vector*.
#[proc_macro_attribute]
pub fn defun(attr_ts: TokenStream, fn_ts: TokenStream) -> TokenStream {
    let function = parse_macro_input!(fn_ts as defun::Function);

    match NestedMeta::parse_meta_list(attr_ts.into()) {
        Ok(args) => match defun::Spec::from_list(&args) {
            Ok(spec) => defun::expand(function, spec).into(),
            Err(e) => TokenStream::from(e.write_errors()),
        },
        Err(e) => TokenStream::from(DError::from(e).write_errors()),
    }
}

#[proc_macro_derive(Trace, attributes(no_trace))]
pub fn derive_trace(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        data,
        generics,
        ..
    } = parse_macro_input!(input);

    match data {
        syn::Data::Struct(data_struct) => derive_trace_struct(ident, data_struct, generics).into(),
        syn::Data::Enum(data_enum) => derive_trace_enum(ident, data_enum, generics).into(),
        _ => panic!("Union types are not supported."),
    }
}

fn derive_trace_struct(
    name: Ident,
    record: DataStruct,
    generics: Generics,
) -> proc_macro2::TokenStream {
    let fields = match record.fields {
        Fields::Named(fields) => fields.named,
        Fields::Unnamed(fields) => fields.unnamed,
        _ => {
            return quote! {
                unsafe impl crate::gc::Trace for #name {
                    unsafe fn trace(&self, visitor: crate::gc::Visitor) {}
                }
            };
        }
    };

    let Generics {
        mut params,
        where_clause,
        ..
    } = generics;

    let mut unbound_params = Punctuated::<GenericParam, Token![,]>::new();

    for param in params.iter_mut() {
        match param {
            GenericParam::Type(ty) => {
                ty.bounds
                    .push(syn::TypeParamBound::Verbatim(quote! { crate::gc::Trace }));
                unbound_params.push(GenericParam::Type(syn::TypeParam::from(ty.ident.clone())));
            }
            param => unbound_params.push(param.clone()),
        }
    }

    let field_visits = fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !no_trace(&f.attrs))
        .map(|(i, f)| {
            let ident = f.ident.clone().map_or_else(
                || {
                    Member::Unnamed(syn::Index {
                        index: i as u32,
                        span: Span::call_site(),
                    })
                },
                Member::Named,
            );
            if is_gc(&f.ty) {
                quote! {
                    visitor(self.#ident.as_opaque());
                }
            } else {
                quote! {
                    self. #ident .trace(visitor);
                }
            }
        })
        .collect::<Vec<_>>();

    let field_drops = fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !no_trace(&f.attrs))
        .flat_map(|(i, f)| {
            let ident = f.ident.clone().map_or_else(
                || {
                    Member::Unnamed(syn::Index {
                        index: i as u32,
                        span: Span::call_site(),
                    })
                },
                Member::Named,
            );
            if !is_gc(&f.ty) {
                Some(quote! {
                        self.#ident.finalize();
                })
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    quote! {
        #[automatically_derived]
        unsafe impl<#params> crate::gc::Trace for #name <#unbound_params>
        #where_clause
        {
            unsafe fn trace(&self, visitor: crate::gc::Visitor) {
                #(
                    #field_visits
                )*
            }

            unsafe fn finalize(&mut self) {
                #(
                    #field_drops
                )*
            }
        }
    }
}

// TODO: Add generics here
fn derive_trace_enum(
    name: Ident,
    data_enum: DataEnum,
    generics: Generics,
) -> proc_macro2::TokenStream {
    let (trace_match_clauses, finalize_match_clauses): (Vec<_>, Vec<_>) = data_enum
        .variants
        .into_iter()
        .flat_map(|variant| {
            let no_trace = no_trace(&variant.attrs);
            if no_trace {
                return None;
            }
            let fields: Vec<_> = match variant.fields {
                Fields::Named(ref named) => named
                    .named
                    .iter()
                    .map(|field| (field.ty.clone(), field.ident.as_ref().unwrap().clone()))
                    .collect(),
                Fields::Unnamed(ref unnamed) => unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, field)| {
                        let ident = Ident::new(&format!("t{i}"), Span::call_site());
                        (field.ty.clone(), ident)
                    })
                    .collect(),
                _ => return None,
            };
            let visits: Vec<_> = fields
                .iter()
                .map(|(ty, accessor)| {
                    if is_gc(ty) {
                        quote! {
                            visitor(#accessor.as_opaque())
                        }
                    } else {
                        quote! {
                            #accessor.trace(visitor)
                        }
                    }
                })
                .collect();
            let drops: Vec<_> = fields
                .iter()
                .filter(|(ty, _)| !is_gc(ty))
                .map(|(_, accessor)| {
                    quote! {
                        #accessor.finalize();
                    }
                })
                .collect();
            let field_name = fields.iter().map(|(_, field)| field);
            let fields_destructured = match variant.fields {
                Fields::Named(..) => quote! { { #( #field_name, )* .. } },
                _ => quote! { ( #( #field_name ),* ) },
            };
            let field_name = fields.iter().map(|(_, field)| field);
            let fields_destructured_mut = match variant.fields {
                Fields::Named(..) => quote! { { #( #field_name, )* .. } },
                _ => quote! { ( #( #field_name ),* ) },
            };
            let variant_name = variant.ident;
            Some((
                quote! {
                    Self::#variant_name #fields_destructured => {
                        #(
                            #visits;
                        )*
                    }
                },
                quote! {
                    Self::#variant_name #fields_destructured_mut => {
                        #(
                            #drops
                        )*
                    }
                },
            ))
        })
        .unzip();

    let Generics {
        mut params,
        where_clause,
        ..
    } = generics;

    let mut unbound_params = Punctuated::<GenericParam, Token![,]>::new();

    for param in params.iter_mut() {
        match param {
            GenericParam::Type(ty) => {
                ty.bounds
                    .push(syn::TypeParamBound::Verbatim(quote! { crate::gc::Trace }));
                unbound_params.push(GenericParam::Type(syn::TypeParam::from(ty.ident.clone())));
            }
            param => unbound_params.push(param.clone()),
        }
    }

    quote! {
        unsafe impl<#params> crate::gc::Trace for #name <#unbound_params>
        #where_clause
        {
            unsafe fn trace(&self, visitor: crate::gc::Visitor) {
                match self {
                    #( #trace_match_clauses, )*
                    _ => (),
                }
            }

            unsafe fn finalize(&mut self) {
                match self {
                    #( #finalize_match_clauses, )*
                    _ => (),
                }
            }
        }
    }
}

fn is_gc(arg: &Type) -> bool {
    if let Type::Path(path) = arg {
        return path
            .path
            .segments
            .last()
            .map(|p| p.ident.to_string())
            .as_deref()
            == Some("Gc");
    }
    false
}

fn no_trace(attrs: &[syn::Attribute]) -> bool {
    for attr in attrs {
        if let syn::Meta::Path(path) = &attr.meta
            && path.is_ident("no_trace")
        {
            return true;
        }
    }
    false
}
