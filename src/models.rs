use std::collections::HashMap;

use darling::util::IdentString;
use darling::{FromDeriveInput, FromField};
use darling::{FromMeta, ast, util};
use quote::ToTokens;
use syn::punctuated::Punctuated;

#[derive(Debug, FromDeriveInput)]
#[darling(
    attributes(table),
    supports(struct_named),
    forward_attrs(allow, doc, cfg)
)]
pub(crate) struct TableRowDeriveInput {
    pub(crate) ident: syn::Ident,
    pub(crate) vis: syn::Visibility,
    pub(crate) data: ast::Data<util::Ignored, TableRowField>,
    pub(crate) generics: syn::Generics,

    #[darling(default)]
    pub(crate) classes_provider: Option<IdentString>,

    #[darling(default)]
    pub(crate) thead_cell_renderer: Option<IdentString>,

    #[darling(default)]
    pub(crate) sortable: bool,

    #[darling(default)]
    pub(crate) impl_vec_data_provider: bool,

    #[darling(default)]
    pub(crate) column_index_type: ColumnIndexType,

    #[darling(default)]
    pub(crate) row_type: Option<syn::Type>,

    #[darling(default)]
    pub(crate) i18n: Option<I18nStructOptions>,
}

/// How to fill in the generic column type.
#[derive(Debug, FromMeta, Default)]
#[darling(rename_all = "lowercase")]
pub(crate) enum ColumnIndexType {
    /// 0-based index based on struct field positions.
    #[default]
    Usize,
    /// Generated enum, variants based on struct field names
    Enum,
}

#[derive(Debug, FromField)]
#[darling(attributes(table))]
pub(crate) struct TableRowField {
    pub(crate) ident: Option<syn::Ident>,
    pub(crate) ty: syn::Type,

    #[darling(default)]
    pub(crate) marker: Option<syn::Ident>,

    #[darling(default)]
    pub(crate) renderer: Option<IdentString>,

    #[darling(default)]
    pub(crate) format: HashMap<syn::Ident, syn::Lit>,

    #[darling(default)]
    pub(crate) class: Option<String>,

    #[darling(default)]
    pub(crate) cell_class: Option<String>,

    #[darling(default)]
    pub(crate) head_class: Option<String>,

    #[darling(default)]
    pub(crate) title: Option<String>,

    #[darling(default)]
    pub(crate) skip: bool,

    #[darling(default)]
    pub(crate) skip_header: bool,

    #[darling(default)]
    pub(crate) skip_sort: bool,

    #[darling(default)]
    pub(crate) getter: Option<IdentString>,

    #[darling(default)]
    pub(crate) none_value: Option<String>,

    #[darling(default)]
    pub(crate) i18n: Option<I18nFieldOptions>,
}

#[derive(Debug, FromMeta)]
pub(crate) struct I18nStructOptions {
    #[darling(default)]
    pub(crate) path: Option<syn::Path>,
    #[darling(default)]
    pub(crate) scope: Option<Punctuated<syn::Ident, syn::Token![.]>>,
}

#[derive(Debug, FromMeta)]
pub(crate) struct I18nFieldOptions {
    #[darling(default)]
    pub(crate) skip: Option<bool>,
    #[darling(default)]
    pub(crate) key: Option<I18nKey>,
}

impl TableRowField {
    pub(crate) fn cell_class(&self) -> String {
        let mut class = "".to_owned();

        if let Some(ref c) = self.class {
            class.push_str(c);
        }
        if let Some(ref c) = self.cell_class {
            class.push(' ');
            class.push_str(c);
        }

        class
    }

    pub(crate) fn head_class(&self) -> String {
        let mut class = "".to_owned();

        if let Some(ref c) = self.class {
            class.push_str(c);
        }
        if let Some(ref c) = self.head_class {
            class.push(' ');
            class.push_str(c);
        }

        class
    }
}

impl I18nFieldOptions {
    pub(crate) fn is_skipped(&self) -> bool {
        self.skip.is_some_and(|v| v)
    }
}

#[derive(Debug)]
pub(crate) struct I18nKey(proc_macro2::TokenStream);

impl ToTokens for I18nKey {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.0.to_tokens(tokens)
    }
}

// This is needed to parse `i18n(key = path.to.translations)`, `syn::Punctuated` does implement `FromMeta` but only if the input is a string.
// We could have `i18n(key = "path.to.translations")` and call it a day, but I prefer without quotes.
impl FromMeta for I18nKey {
    fn from_meta(item: &syn::Meta) -> darling::Result<Self> {
        let res: darling::Result<Self> = match item {
            syn::Meta::NameValue(syn::MetaNameValue { value, .. }) => {
                Ok(I18nKey(value.to_token_stream()))
            }
            _ => Err(darling::Error::custom(
                "Providing the i18n key only support the i18n(key = path.to.translations) form, i18n(key) and i18n(key(.., ..)) are not supported.",
            )),
        };
        res.map_err(|e| e.with_span(item))
    }
}
