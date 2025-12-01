extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput};
use syn::{Type, TypePath};

fn is_option(ty: &syn::Type) -> bool {
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            return segment.ident == "Option";
        }
    }
    false
}

fn is_hashmap(ty: &syn::Type) -> bool {
    // Match any type whose last path segment is "HashMap"
    // This follows the same pattern as serde - if someone uses a custom HashMap,
    // they'll get a compile error when the generated code tries to use HashMap::new()
    if let Type::Path(TypePath { path, .. }) = ty {
        if let Some(segment) = path.segments.last() {
            return segment.ident == "HashMap";
        }
    }
    false
}

fn get_inner_type_if_option(ty: &syn::Type) -> &syn::Type {
    if !is_option(ty) {
        return ty;
    }

    let Type::Path(TypePath { path, .. }) = ty else {
        return ty;
    };

    path.segments
        .last()
        .and_then(|seg| match &seg.arguments {
            syn::PathArguments::AngleBracketed(args) => args.args.first(),
            _ => None,
        })
        .and_then(|arg| match arg {
            syn::GenericArgument::Type(inner) => Some(inner),
            _ => None,
        })
        .unwrap_or(ty)
}

// Generate `from_paragraph`, ``to_paragraph`` methods for the annotated struct, i.e.:
//
// ```rust
// #[derive(FromDeb822)]
// struct X {
//    a: i32,
//    b: i32,
//    c: Option<String>,
//    d: Vec<String>,
//    #[deb822(field = "E")]
//    e: bool,
// }
// ```
//
// will generate:
//
// ```rust
//
// impl<P: deb822_fast::convert::Deb822LikeParagraph> FromDeb822Paragraph<P> for X {
//     fn from_paragraph(para: &P) -> Result<Self, String> {
//     Ok(Self {
//         a: para.get("a").ok_or_else(|| "missing field: a")?.parse().map_err(|e| format!("parsing field a: {}", e))?,
//         b: para.get("b").ok_or_else(|| "missing field: b")?.parse().map_err(|e| format!("parsing field b: {}", e))?,
//         c: para.get("c").map(|v| v.parse().map_err(|e| format!("parsing field c: {}", e))).transpose()?,
//         d: para.get("d").ok_or_else(|| "missing field: d")?.split_whitespace().map(|s| s.to_string()).collect(),
//         e: para.get("E").ok_or_else(|| "missing field: e")?.parse().map_err(|e| format!("parsing field E: {}", e))?,
//     })
// }
//
// And:
//
// ```rust
// #[derive(ToDeb822)]
// struct X {
//    a: i32,
//    b: i32,
//    c: Option<String>,
//    d: Vec<String>,
//    #[deb822(field = "E")]
//    e: bool,
// }
// ```
//
// will generate:
//
// ```rust
// impl<P: deb822_fast::convert::Deb822LikeParagraph> ToDeb822Paragraph<P> for X {
//     fn to_paragraph(&self) -> P {
//         let mut fields = Vec::<(String, String)>::new();
//         fields.set("a", self.a.to_string());
//         fields.set("b", self.b.to_string());
//         if let Some(v) = &self.c {
//             fields.set("c", v.to_string());
//         }
//         fields.set("d", self.d.join(" "));
//         fields.set("E", self.e.to_string());
//         deb822_fast::Paragraph::from(fields)
//     }
//
//     fn update_paragraph(&self, para: &mut deb822_fast::Paragraph) {
//         para.set("a", &self.a.to_string());
//         para.set("b", &self.b.to_string());
//         if let Some(v) = &self.c {
//             para.set("c", &v.to_string());
//         } else {
//             para.remove("c");
//         }
//         para.set("d", &self.d.join(" "));
//         para.set("E", &self.e.to_string());
//     }
// }
// ```

struct FieldAttributes {
    field: Option<String>,
    prefix: Option<String>,
    serialize_with: Option<syn::ExprPath>,
    deserialize_with: Option<syn::ExprPath>,
}

fn extract_field_attributes(attrs: &[syn::Attribute]) -> Result<FieldAttributes, syn::Error> {
    let mut field = None;
    let mut prefix = None;
    let mut serialize_with = None;
    let mut deserialize_with = None;
    for attr in attrs {
        if !attr.path().is_ident("deb822") {
            continue;
        }
        let name_values: syn::punctuated::Punctuated<syn::MetaNameValue, syn::Token![,]> =
            attr.parse_args_with(syn::punctuated::Punctuated::parse_terminated)?;
        for nv in name_values {
            if nv.path.is_ident("field") {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = nv.value
                {
                    field = Some(s.value());
                } else {
                    return Err(syn::Error::new(
                        nv.value.span(),
                        "expected string literal in deb822 attribute",
                    ));
                }
            } else if nv.path.is_ident("prefix") {
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(s),
                    ..
                }) = nv.value
                {
                    prefix = Some(s.value());
                } else {
                    return Err(syn::Error::new(
                        nv.value.span(),
                        "expected string literal in deb822 attribute",
                    ));
                }
            } else if nv.path.is_ident("serialize_with") {
                if let syn::Expr::Path(s) = nv.value {
                    serialize_with = Some(s);
                } else {
                    return Err(syn::Error::new(
                        nv.value.span(),
                        "expected path in deb822 attribute",
                    ));
                }
            } else if nv.path.is_ident("deserialize_with") {
                if let syn::Expr::Path(s) = nv.value {
                    deserialize_with = Some(s);
                } else {
                    return Err(syn::Error::new(
                        nv.value.span(),
                        "expected path in deb822 attribute",
                    ));
                }
            } else {
                return Err(syn::Error::new(
                    nv.span(),
                    format!("unsupported attribute: {}", nv.path.get_ident().unwrap()),
                ));
            }
        }
    }
    Ok(FieldAttributes {
        field,
        prefix,
        serialize_with,
        deserialize_with,
    })
}

#[proc_macro_derive(FromDeb822, attributes(deb822))]
pub fn derive_from_deb822(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let s = if let syn::Data::Struct(s) = &input.data {
        s
    } else {
        panic!("FromDeb822 can only be derived for structs")
    };

    // Collect field information for building the match statement
    #[derive(Clone)]
    struct FieldInfo {
        ident: syn::Ident,
        field_name: Option<String>, // Exact field name match
        prefix: Option<String>,     // Prefix match
        deserialize_with: proc_macro2::TokenStream,
        is_option: bool,
        is_hashmap: bool,
    }

    // Collect field metadata
    let field_infos: Vec<FieldInfo> = s
        .fields
        .iter()
        .map(|f| {
            let attrs = extract_field_attributes(&f.attrs).unwrap();
            let ident = f.ident.as_ref().unwrap().clone();
            let ty = &f.ty;
            let is_option = is_option(ty);
            let inner_ty = get_inner_type_if_option(ty);

            let has_custom_deserializer = attrs.deserialize_with.is_some();
            let is_hashmap = is_hashmap(inner_ty) && !has_custom_deserializer;

            let deserialize_with = attrs
                .deserialize_with
                .map(|dw| quote! { #dw })
                .unwrap_or_else(|| quote! { std::str::FromStr::from_str });

            let field_name = if attrs.prefix.is_none() && (!is_hashmap || has_custom_deserializer) {
                Some(attrs.field.unwrap_or_else(|| ident.to_string()))
            } else {
                attrs.field
            };

            FieldInfo {
                ident,
                field_name,
                prefix: attrs.prefix,
                deserialize_with,
                is_option,
                is_hashmap,
            }
        })
        .collect();

    // Sort by specificity for match arm ordering
    let mut sorted_infos = field_infos.clone();
    sorted_infos.sort_by(
        |a, b| match (&a.field_name, &a.prefix, &b.field_name, &b.prefix) {
            (Some(_), _, Some(_), _) => std::cmp::Ordering::Equal,
            (Some(_), _, _, _) => std::cmp::Ordering::Less,
            (_, _, Some(_), _) => std::cmp::Ordering::Greater,
            (_, Some(ap), _, Some(bp)) => bp.len().cmp(&ap.len()),
            (_, Some(_), _, None) => std::cmp::Ordering::Less,
            (_, None, _, Some(_)) => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        },
    );

    // Generate initializations
    let inits: Vec<_> = field_infos
        .iter()
        .map(|info| {
            let ident = &info.ident;
            if info.is_hashmap {
                quote! { let mut #ident = std::collections::HashMap::new(); }
            } else {
                quote! { let mut #ident = None; }
            }
        })
        .collect();

    // Generate match arms
    let has_catchall = field_infos
        .iter()
        .any(|info| info.is_hashmap && info.prefix.is_none() && info.field_name.is_none());

    let match_arms: Vec<_> = sorted_infos
        .iter()
        .filter_map(|info| {
            let ident = &info.ident;
            let deserialize = &info.deserialize_with;

            if let Some(exact_field) = &info.field_name {
                Some(quote! {
                    #exact_field => {
                        #ident = Some(#deserialize(&v).map_err(|e| format!("parsing field {}: {}", k, e))?);
                    }
                })
            } else if let Some(prefix) = &info.prefix {
                Some(quote! {
                    s if s.starts_with(#prefix) => {
                        let key_part = &s[#prefix.len()..];
                        let value = #deserialize(&v).map_err(|e| format!("parsing field {}: {}", k, e))?;
                        #ident.insert(key_part.to_string(), value);
                    }
                })
            } else if info.is_hashmap {
                Some(quote! {
                    _ => {
                        let value = #deserialize(&v).map_err(|e| format!("parsing field {}: {}", k, e))?;
                        #ident.insert(k.clone(), value);
                    }
                })
            } else {
                None
            }
        })
        .chain((!has_catchall).then(|| quote! { _ => {} }))
        .collect();

    // Generate field assignments
    let from_fields: Vec<_> = field_infos
        .iter()
        .map(|info| {
            let ident = &info.ident;
            if info.is_hashmap {
                if info.is_option {
                    quote! { #ident: if #ident.is_empty() { None } else { Some(#ident) } }
                } else {
                    quote! { #ident }
                }
            } else if info.is_option {
                quote! { #ident }
            } else {
                let field_name = info.field_name.as_ref().unwrap();
                quote! { #ident: #ident.ok_or_else(|| format!("missing field: {}", #field_name))? }
            }
        })
        .collect();

    let gen = quote! {
        impl<P: deb822_fast::convert::Deb822LikeParagraph> deb822_fast::FromDeb822Paragraph<P> for #name {
            fn from_paragraph(para: &P) -> Result<Self, String> {
                #(#inits)*

                for (k, v) in para.iter() {
                    match k.as_str() {
                        #(#match_arms)*
                    }
                }

                Ok(Self {
                    #(#from_fields,)*
                })
            }
        }
    };
    gen.into()
}

#[proc_macro_derive(ToDeb822, attributes(deb822))]
pub fn derive_to_deb822(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let s = if let syn::Data::Struct(s) = &input.data {
        s
    } else {
        panic!("Deb822 can only be derived for structs")
    };

    let mut to_fields = vec![];
    let mut update_fields = vec![];

    for f in s.fields.iter() {
        let attrs = extract_field_attributes(&f.attrs).unwrap();
        let ident = &f.ident;
        let ty = &f.ty;
        let is_option = is_option(ty);
        let inner_ty = get_inner_type_if_option(ty);

        // Check if this is a HashMap with a prefix
        if let Some(prefix) = attrs.prefix {
            if !is_hashmap(inner_ty) {
                panic!("prefix attribute can only be used with HashMap types");
            }

            let serialize_with = attrs
                .serialize_with
                .map(|sw| quote! { #sw })
                .unwrap_or_else(|| quote! { ToString::to_string });

            if is_option {
                to_fields.push(quote! {
                    if let Some(ref map) = self.#ident {
                        for (k, v) in map.iter() {
                            fields.push((format!("{}{}", #prefix, k), #serialize_with(v)));
                        }
                    }
                });

                update_fields.push(quote! {
                    // Remove all fields matching the prefix
                    let keys_to_remove: Vec<String> = para.iter()
                        .filter(|(k, _)| k.starts_with(#prefix))
                        .map(|(k, _)| k)
                        .collect();
                    for k in keys_to_remove {
                        para.remove(&k);
                    }
                    // Add new fields from the HashMap
                    if let Some(ref map) = self.#ident {
                        for (k, v) in map.iter() {
                            para.set(&format!("{}{}", #prefix, k), #serialize_with(v).as_str());
                        }
                    }
                });
            } else {
                to_fields.push(quote! {
                    for (k, v) in self.#ident.iter() {
                        fields.push((format!("{}{}", #prefix, k), #serialize_with(v)));
                    }
                });

                update_fields.push(quote! {
                    // Remove all fields matching the prefix
                    let keys_to_remove: Vec<String> = para.iter()
                        .filter(|(k, _)| k.starts_with(#prefix))
                        .map(|(k, _)| k)
                        .collect();
                    for k in keys_to_remove {
                        para.remove(&k);
                    }
                    // Add new fields from the HashMap
                    for (k, v) in self.#ident.iter() {
                        para.set(&format!("{}{}", #prefix, k), #serialize_with(v).as_str());
                    }
                });
            }
            continue;
        }

        // Check if this is a catch-all HashMap (no prefix or field attribute)
        if is_hashmap(inner_ty) && attrs.field.is_none() {
            let serialize_with = attrs
                .serialize_with
                .map(|sw| quote! { #sw })
                .unwrap_or_else(|| quote! { ToString::to_string });

            if is_option {
                to_fields.push(quote! {
                    if let Some(ref map) = self.#ident {
                        for (k, v) in map.iter() {
                            fields.push((k.clone(), #serialize_with(v)));
                        }
                    }
                });

                update_fields.push(quote! {
                    // For catch-all, we need to remove fields that aren't explicitly handled
                    // This is complex - for now just add the catch-all fields
                    if let Some(ref map) = self.#ident {
                        for (k, v) in map.iter() {
                            para.set(k, #serialize_with(v).as_str());
                        }
                    }
                });
            } else {
                to_fields.push(quote! {
                    for (k, v) in self.#ident.iter() {
                        fields.push((k.clone(), #serialize_with(v)));
                    }
                });

                update_fields.push(quote! {
                    // For catch-all, just add all the fields
                    for (k, v) in self.#ident.iter() {
                        para.set(k, #serialize_with(v).as_str());
                    }
                });
            }
            continue;
        }

        // Regular field handling
        let key = attrs
            .field
            .unwrap_or_else(|| ident.as_ref().unwrap().to_string());
        let serialize_with = attrs
            .serialize_with
            .map(|sw| quote! { #sw })
            .unwrap_or_else(|| quote! { ToString::to_string });

        to_fields.push(if is_option {
            quote! {
                if let Some(v) = &self.#ident {
                    fields.push((#key.to_string(), #serialize_with(&v)));
                }
            }
        } else {
            quote! {
                fields.push((#key.to_string(), #serialize_with(&self.#ident)));
            }
        });

        update_fields.push(if is_option {
            quote! {
                if let Some(v) = &self.#ident {
                    para.set(#key, #serialize_with(&v).as_str());
                } else {
                    para.remove(#key);
                }
            }
        } else {
            quote! {
                para.set(#key, #serialize_with(&self.#ident).as_str());
            }
        });
    }

    let gen = quote! {
        impl<P: deb822_fast::convert::Deb822LikeParagraph> deb822_fast::ToDeb822Paragraph<P> for #name {
            fn to_paragraph(&self) -> P {
                let mut fields = Vec::<(String, String)>::new();
                #(#to_fields)*
                fields.into_iter().collect()
            }

            fn update_paragraph(&self, para: &mut P) {
                #(#update_fields)*
            }
        }
    };
    gen.into()
}
