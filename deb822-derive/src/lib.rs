extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
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

/// Generate code to format a field value based on its FieldType
fn apply_field_formatting(
    field_type: Option<FieldType>,
    field_name: &str,
) -> proc_macro2::TokenStream {
    match field_type {
        Some(FieldType::SingleLine) => quote! {
            deb822_fast::convert::format_single_line(&value, #field_name)
        },
        Some(FieldType::MultiLine) => quote! {
            deb822_fast::convert::format_multi_line(&value)
        },
        Some(FieldType::Folded) => quote! {
            deb822_fast::convert::format_folded(&value)
        },
        None => quote! { value },
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FieldType {
    SingleLine,
    MultiLine,
    Folded,
}

struct FieldAttributes {
    field: Option<String>,
    serialize_with: Option<syn::ExprPath>,
    deserialize_with: Option<syn::ExprPath>,
    field_type: Option<FieldType>,
}

fn extract_field_attributes(attrs: &[syn::Attribute]) -> Result<FieldAttributes, syn::Error> {
    let mut field = None;
    let mut serialize_with = None;
    let mut deserialize_with = None;
    let mut field_type = None;

    for attr in attrs {
        if !attr.path().is_ident("deb822") {
            continue;
        }

        // Parse the attribute arguments
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("field") {
                let value = meta.value()?;
                let s: syn::LitStr = value.parse()?;
                field = Some(s.value());
                Ok(())
            } else if meta.path.is_ident("serialize_with") {
                let value = meta.value()?;
                let path: syn::ExprPath = value.parse()?;
                serialize_with = Some(path);
                Ok(())
            } else if meta.path.is_ident("deserialize_with") {
                let value = meta.value()?;
                let path: syn::ExprPath = value.parse()?;
                deserialize_with = Some(path);
                Ok(())
            } else if meta.path.is_ident("folded") {
                if field_type.is_some() {
                    return Err(meta.error(
                        "only one of 'folded', 'single_line', or 'multi_line' can be specified",
                    ));
                }
                field_type = Some(FieldType::Folded);
                Ok(())
            } else if meta.path.is_ident("single_line") {
                if field_type.is_some() {
                    return Err(meta.error(
                        "only one of 'folded', 'single_line', or 'multi_line' can be specified",
                    ));
                }
                field_type = Some(FieldType::SingleLine);
                Ok(())
            } else if meta.path.is_ident("multi_line") {
                if field_type.is_some() {
                    return Err(meta.error(
                        "only one of 'folded', 'single_line', or 'multi_line' can be specified",
                    ));
                }
                field_type = Some(FieldType::MultiLine);
                Ok(())
            } else {
                Err(meta.error(format!(
                    "unsupported attribute: {}",
                    meta.path.get_ident().unwrap()
                )))
            }
        })?;
    }

    Ok(FieldAttributes {
        field,
        serialize_with,
        deserialize_with,
        field_type,
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

    let from_fields = s.fields.iter().map(|f| {
        let attrs = extract_field_attributes(&f.attrs).unwrap();
            let ident = &f.ident;
            // Get key either from the #[deb822(field = "foo")] attribute, or derive it from the
            // field name
            let key = attrs.field.unwrap_or_else(||ident.as_ref().unwrap().to_string());
            let deserialize_with = if let Some(deserialize_with) = attrs.deserialize_with {
                quote! { #deserialize_with }
            } else {
                quote! { std::str::FromStr::from_str }
            };
            // Check if the field is optional or not
            let ty = &f.ty;
            let is_option = is_option(ty);

            if is_option {
                // Allow the field to be missing
                quote! {
                    #ident: para.get(#key).map(|v| #deserialize_with(&v).map_err(|e| format!("parsing field {}: {}", #key, e))).transpose()?
                }
            } else {
                // The field is required
                quote! {
                    #ident: #deserialize_with(&para.get(#key).ok_or_else(|| format!("missing field: {}", #key))?).map_err(|e| format!("parsing field {}: {}", #key, e))?
                }
            }
        }).collect::<Vec<_>>();

    let gen = quote! {
        impl<P: deb822_fast::convert::Deb822LikeParagraph> deb822_fast::FromDeb822Paragraph<P> for #name {
            fn from_paragraph(para: &P) -> Result<Self, String> {
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
        let key = attrs
            .field
            .unwrap_or_else(|| ident.as_ref().unwrap().to_string());
        let serialize_with = if let Some(serialize_with) = attrs.serialize_with {
            quote! { #serialize_with }
        } else {
            quote! { ToString::to_string }
        };

        let field_type = attrs.field_type;
        let format_value = apply_field_formatting(field_type, &key);

        let ty = &f.ty;
        let is_option = is_option(ty);

        to_fields.push(if is_option {
            quote! {
                if let Some(v) = &self.#ident {
                    let value = #serialize_with(&v);
                    let formatted = #format_value;
                    fields.push((#key.to_string(), formatted));
                }
            }
        } else {
            quote! {
                let value = #serialize_with(&self.#ident);
                let formatted = #format_value;
                fields.push((#key.to_string(), formatted));
            }
        });

        update_fields.push(if is_option {
            quote! {
                if let Some(v) = &self.#ident {
                    let value = #serialize_with(&v);
                    let formatted = #format_value;
                    para.set(#key, formatted.as_str());
                } else {
                    para.remove(#key);
                }
            }
        } else {
            quote! {
                let value = #serialize_with(&self.#ident);
                let formatted = #format_value;
                para.set(#key, formatted.as_str());
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
