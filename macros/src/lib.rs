extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{parse_macro_input, Data, DeriveInput, Expr, ExprPath, FieldValue, FnArg, Ident, Lit, Member, Meta, Pat, PatIdent, PatType, Token};
use syn::spanned::Spanned;
use quote::quote;
use syn::punctuated::{Pair, Punctuated};

/// The constructor macro. Takes in a struct with multiple fields and creates a `new` function with these attributes.
/// You can rename the function to whatever you want by setting `#[ctor_name = "new_name"]`.
/// You can also change the calling names of any of the fields by using `#[ctor_rename]` on a field.
#[proc_macro_derive(Constructor, attributes(ctor_name, ctor_rename))]
pub fn constructor_macro(input: TokenStream) -> TokenStream {
    let derived_input = parse_macro_input!(input as DeriveInput);
    let name = &derived_input.ident;
    let generics = &derived_input.generics;
    let fn_name = match derived_input.attrs.iter().find(|attr| {attr.path().is_ident("ctor_name")}) {
        Some(f) => {
            match &f.meta {
                Meta::NameValue(m) => {
                    let Expr::Lit(lit) = &m.value else {
                        return syn::Error::new(f.span(), "Arguments passed to #[ctor_name] must be string literals.").to_compile_error().into();
                    };
                    let Lit::Str(lit_str) = &lit.lit else {
                        return syn::Error::new(f.span(), "Expected string literal.").to_compile_error().into();
                    };
                    Ident::new(&*lit_str.value(), derived_input.span())
                }
                _ => return syn::Error::new(f.span(),"Incorrect use of #[ctor_name], use #[ctor_name = \"your_name\"] instead." ).to_compile_error().into(),
            }
        },
        None => Ident::new(&*String::from("new"), derived_input.span())
    };
    if let Data::Struct(ref struct_data) = derived_input.data {
        match struct_data.fields {
            syn::Fields::Named(ref fields) => {
                let mut var_names = Vec::new();
                for field in fields.named.iter() {
                    if let Some(attr) = field.attrs.iter().find(|attr| attr.path().is_ident("ctor_rename")) {
                        let Meta::NameValue(name_value) = &attr.meta else {
                            return syn::Error::new(field.span(),"Expected #[ctor_rename] attribute to have a name_value attached, like #[ctor_rename = \"string\"").to_compile_error().into();
                        };
                        let Expr::Lit(lit) = name_value.value.clone() else {
                            return syn::Error::new(field.span(),"Incorrect use of #[ctor_name] attribute.").to_compile_error().into();
                        };
                        let Lit::Str(litstr) = lit.lit else {
                            return syn::Error::new(field.span(), "Incorrect use of #[ctor_name] attribute, expected string literal").to_compile_error().into();
                        };
                        var_names.push(Ident::new(&*litstr.value(), field.ident.span()));
                    } else {
                        var_names.push(field.ident.clone().unwrap())
                    }
                };
                let field_names = fields.named.iter().map(|f| f.ident.clone().unwrap()).collect::<Vec<_>>();
                let name_value_pairs: Punctuated<FieldValue, Token![,]> = Punctuated::from_iter(field_names.iter().enumerate().map(|(idx, ident)| {
                    let field_value = FieldValue {
                        attrs: Vec::new(),
                        member: Member::Named(ident.clone()),
                        colon_token: Some(Token![:](syn::spanned::Spanned::span(&ident))),
                        expr: Expr::Path(ExprPath {
                            attrs: Vec::new(),
                            qself: None,
                            path: var_names[idx].clone().into()
                        })
                    };
                    if idx < field_names.len() - 1 {
                        Pair::Punctuated(field_value, syn::token::Comma::default())
                    } else {
                        Pair::End(field_value)
                    }
                }));
                let type_value_pairs: Punctuated<FnArg, Token![,]> = Punctuated::from_iter(var_names.iter().enumerate().map(|(idx, ident)| {
                    let fn_arg = FnArg::Typed(PatType {
                        attrs: Vec::new(),
                        pat: Box::new(Pat::Ident(PatIdent {
                            attrs: Vec::new(),
                            by_ref: None,
                            mutability: None,
                            ident: ident.clone(),
                            subpat: None
                        })),
                        colon_token: syn::token::Colon::default(),
                        ty: Box::new(fields.named[idx].ty.clone())
                    });
                    if idx < var_names.len() - 1 {
                        Pair::Punctuated(fn_arg, syn::token::Comma::default())
                    } else {
                        Pair::End(fn_arg)
                    }
                }));
                if generics.params.is_empty() {
                    quote! {
                        impl #name {
                            fn #fn_name(#type_value_pairs) -> Self {
                                Self {
                                    #name_value_pairs
                                }
                            }
                        }
                    }.into()
                } else {
                    let params = generics.params.clone();
                    if let Some(where_clause) = generics.where_clause.clone() {
                        quote! {
                            impl<#params> #name<#params> #where_clause {
                                fn #fn_name(#type_value_pairs) -> Self {
                                    Self {
                                        #name_value_pairs
                                    }
                                }
                            }
                        }.into()
                    } else {
                        quote! {
                            impl<#params> #name<#params> {
                                fn #fn_name(#type_value_pairs) -> Self {
                                    Self {
                                        #name_value_pairs
                                    }
                                }
                            }
                        }.into()
                    }
                }
            },
            syn::Fields::Unnamed(ref fields) => {
                let mut var_names = Vec::new();
                for (idx, field) in fields.unnamed.iter().enumerate() {
                    if let Some(attr) = field.attrs.iter().find(|attr| attr.path().is_ident("ctor_rename")) {
                        let Meta::NameValue(name_value) = attr.meta.clone() else {
                            return syn::Error::new(field.span(),"Expected #[ctor_name] attribute.").to_compile_error().into();
                        };
                        let Expr::Lit(lit) = name_value.value else {
                            return syn::Error::new(field.span(),"Incorrect use of #[ctor_name] attribute.").to_compile_error().into();
                        };
                        let Lit::Str(litstr) = lit.lit else {
                            return syn::Error::new(field.span(), "Incorrect use of #[ctor_name] attribute, expected string literal").to_compile_error().into();
                        };
                        var_names.push(Ident::new(&*litstr.value(), field.ident.span()));
                    } else {
                        var_names.push(Ident::new(&*format!("value_{}", idx + 1), fields.span()));
                    }
                };
                let punctuated_var_names: Punctuated<&Ident, Token![,]> = Punctuated::from_iter(var_names.iter().enumerate().map(|(idx, ident)| {
                    if idx < var_names.len() - 1 {
                        Pair::Punctuated(ident, syn::token::Comma::default())
                    } else {
                        Pair::End(ident)
                    }
                }));
                let type_value_pairs = Punctuated::<FnArg, Token![,]>::from_iter(var_names.iter().enumerate().map(|(idx, ident)| {
                    let fn_arg = FnArg::Typed(PatType {
                        attrs: Vec::new(),
                        pat: Box::new(Pat::Ident(PatIdent {
                            attrs: Vec::new(),
                            by_ref: None,
                            mutability: None,
                            ident: ident.clone(),
                            subpat: None
                        })),
                        colon_token: syn::token::Colon::default(),
                        ty: Box::new(fields.unnamed[idx].ty.clone())
                    });
                    if idx < var_names.len() - 1 {
                        Pair::Punctuated(fn_arg, syn::token::Comma::default())
                    } else {
                        Pair::End(fn_arg)
                    }
                }));
                if generics.params.is_empty() {
                    quote! {
                        impl #name {
                            fn #fn_name(#type_value_pairs) -> Self {
                                Self(#punctuated_var_names)
                            }
                        }
                    }.into()
                } else {
                    let params = generics.params.clone();
                    if let Some(where_clause) = generics.where_clause.clone() {
                        quote! {
                            impl<#params> #name<#params> #where_clause {
                                fn #fn_name(#type_value_pairs) -> Self {
                                    Self(#punctuated_var_names)
                                }
                            }
                        }.into()
                    } else {
                        quote! {
                            impl<#params> #name<#params> {
                                fn #fn_name(#type_value_pairs) -> Self {
                                    Self(#punctuated_var_names)
                                }
                            }
                        }.into()
                    }
                }
            },
            syn::Fields::Unit => {
                quote! {
                    impl #name {
                        fn #fn_name() -> Self {
                            Self
                        }
                    }
                }.into()
            }
        }
    } else {
        syn::Error::new(name.span(), "Cannot derive constructor for #[derive(Constructor)] if type is not a struct").to_compile_error().into()
    }
}