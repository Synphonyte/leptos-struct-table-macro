use crate::models::{I18nFieldOptions, TableRowDeriveInput, TableRowField};
use darling::util::IdentString;
use heck::ToTitleCase;
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::{Error, PathSegment, Type, WhereClause, __private::TokenStream2};

fn get_default_renderer_for_field_getter(
    class_prop: &TokenStream,
    value_prop: &TokenStream,
    index_prop: &TokenStream,
    segment: &PathSegment,
    field: &TableRowField,
    getter: &TokenStream2,
) -> TokenStream {
    match get_inner_type(segment, "FieldGetter") {
        Ok(type_ident) => get_default_renderer_for_type(
            class_prop, value_prop, index_prop, type_ident, field, getter,
        ),
        Err(err) => err.to_compile_error(),
    }
}

fn get_default_render_for_inner_type(
    class_prop: &TokenStream,
    value_prop: &TokenStream2,
    index_prop: &TokenStream,
    field: &TableRowField,
    type_ident: &syn::Type,
) -> TokenStream {
    let format_props = get_format_props_for_field(field, type_ident);
    let marker = field.marker.as_ref().map_or_else(
        || get_default_cell_value_marker(type_ident),
        |marker| quote! { #marker },
    );

    quote! {
        <leptos_struct_table::DefaultTableCellRenderer<_, #type_ident, #marker> options=#format_props #value_prop #class_prop #index_prop row=row />
    }
}

// TODO: Code duplication with get_field_getter_inner_type --> could be merged in one function
fn get_inner_type<'a>(
    segment: &'a PathSegment,
    outer_type_name: &str,
) -> Result<&'a syn::Type, syn::Error> {
    let error_message = format!("`{outer_type_name}` should have one type argument");

    if let syn::PathArguments::AngleBracketed(arg) = &segment.arguments {
        if arg.args.len() != 1 {
            return Err(Error::new_spanned(&segment.ident, &error_message));
        }

        let arg = arg.args.first().expect("just checked above");

        if let syn::GenericArgument::Type(ty) = arg {
            Ok(ty)
        } else {
            Err(Error::new_spanned(&segment.ident, &error_message))
        }
    } else {
        Err(Error::new_spanned(&segment.ident, &error_message))
    }
}

fn get_default_option_renderer(
    class_prop: &TokenStream,
    index_prop: &TokenStream,
    type_ident: &syn::Type,
    field: &TableRowField,
    getter: &TokenStream2,
) -> TokenStream {
    if let Type::Path(path) = &field.ty {
        let last_segment = path.path.segments.last().expect("not empty");

        return match get_inner_type(last_segment, "Option") {
            Ok(inner_type_ident) => {
                let value_prop = quote! {
                    value=leptos::prelude::Signal::derive(move || value.get().expect("Just checked above that it's not None"))
                };

                let none_value = field.none_value.clone().unwrap_or_default();

                let inner_renderer = get_default_render_for_inner_type(
                    class_prop,
                    &value_prop,
                    index_prop,
                    field,
                    inner_type_ident,
                );

                quote! {
                    {
                        use leptos::prelude::Read;

                        let value = leptos::prelude::Signal::derive(move || { row.read().#getter });

                        leptos::prelude::view! {
                            <leptos::control_flow::Show
                                when={
                                    move || { value.read().is_some() }
                                }
                                fallback=move || {
                                    type DefaultMarker = ();
                                    leptos::view! {
                                        <leptos_struct_table::DefaultTableCellRenderer<_, String, DefaultMarker>
                                            value=leptos::prelude::Signal::stored(#none_value.to_string())
                                            options={()}
                                            #class_prop #index_prop row=row
                                        />
                                    }
                                }
                            >
                                #inner_renderer
                            </leptos::control_flow::Show>
                        }
                    }
                }
            }
            Err(err) => err.to_compile_error(),
        };
    }

    Error::new_spanned(type_ident, "Invalid Option type").to_compile_error()
}

fn is_option(ty: &syn::Type) -> bool {
    if let syn::Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Option" {
                return true;
            }
        }
    }
    false
}

fn get_default_renderer_for_type(
    class_prop: &TokenStream,
    value_prop: &TokenStream,
    index_prop: &TokenStream,
    type_ident: &syn::Type,
    field: &TableRowField,
    getter: &TokenStream2,
) -> TokenStream {
    if is_option(type_ident) {
        get_default_option_renderer(class_prop, index_prop, type_ident, field, getter)
    } else {
        get_default_render_for_inner_type(class_prop, value_prop, index_prop, field, type_ident)
    }
}

fn get_format_props_for_field(field: &TableRowField, ty: &syn::Type) -> TokenStream2 {
    let values: Vec<_> = field
        .format
        .iter()
        .map(|(ident, value)| {
            quote! {o.#ident = Some(#value.into());}
        })
        .collect();
    let marker = field.marker.as_ref().map_or_else(
        || get_default_cell_value_marker(ty),
        |marker| quote! { #marker },
    );

    quote! {
        {
            type DefaultMarker = ();
            let mut o = <#ty as ::leptos_struct_table::CellValue<#marker>>::RenderOptions::default();
            #(#values)*
            o
      }
    }
}

fn get_default_cell_value_marker(ty: &syn::Type) -> TokenStream2 {
    match ty {
        Type::Path(path) => {
            let name = path
                .path
                .segments
                .last()
                .expect("not empty")
                .ident
                .to_string();
            match &*name {
                "&String" => quote! { &String },
                "i8" => quote! { i8 },
                "i16" => quote! { i16 },
                "i32" => quote! { i32 },
                "i64" => quote! { i64 },
                "i128" => quote! { i128 },
                "isize" => quote! { isize },
                "u8" => quote! { u8 },
                "u16" => quote! { u16 },
                "u32" => quote! { u32 },
                "u64" => quote! { u64 },
                "u128" => quote! { u128 },
                "usize" => quote! { usize },
                "f32" => quote! { f32 },
                "f64" => quote! { f64 },
                "bool" => quote! { bool },
                "char" => quote! { char },
                "IpAddr" => quote! { IpAddr },
                "Ipv4Addr" => quote! { Ipv4Addr },
                "Ipv6Addr" => quote! { Ipv6Addr },
                "SocketAddr" => quote! { SocketAddr },
                "SocketAddrV4" => quote! { SocketAddrV4 },
                "SocketAddrV6" => quote! { SocketAddrV6 },
                "ToUpperCase" => quote! { ToUpperCase },
                "ToLowerCase" => quote! { ToLowerCase },
                "NonZeroI8" => quote! { NonZeroI8 },
                "NonZeroI16" => quote! { NonZeroI16 },
                "NonZeroI32" => quote! { NonZeroI32 },
                "NonZeroI64" => quote! { NonZeroI64 },
                "NonZeroI128" => quote! { NonZeroI128 },
                "NonZeroIsize" => quote! { NonZeroIsize },
                "NonZeroU8" => quote! { NonZeroU8 },
                "NonZeroU16" => quote! { NonZeroU16 },
                "NonZeroU32" => quote! { NonZeroU32 },
                "NonZeroU64" => quote! { NonZeroU64 },
                "NonZeroU128" => quote! { NonZeroU128 },
                "NonZeroUsize" => quote! { NonZeroUsize },
                "NaiveDate" => quote! { NaiveDate },
                "NaiveDateTime" => quote! { NaiveDateTime },
                "NaiveTime" => quote! { NaiveTime },
                "Decimal" => quote! { Decimal },
                "Time" => quote! { Time },
                "Date" => quote! { Date },
                "PrimitiveDateTime" => quote! { PrimitiveDateTime },
                "OffsetDateTime" => quote! { OffsetDateTime },
                "Uuid" => quote! { Uuid },
                _ => quote! { DefaultMarker },
            }
        }
        _ => quote! { DefaultMarker },
    }
}

fn get_renderer_for_field(name: &Ident, field: &TableRowField, index: usize) -> TokenStream2 {
    let getter = get_getter(name, &field.getter, &field.ty);

    let index_prop = quote! {
        index=#index
    };

    let class = field.cell_class();
    let class_prop = quote! { class=class_provider.cell( # class) };

    let value_prop = quote! {
        value={
            use leptos::prelude::Read;

            leptos::prelude::Signal::derive(move || row.read().#getter)
        }
    };

    if let Some(renderer) = &field.renderer {
        let ident = renderer.as_ident();
        quote! {
            <#ident #value_prop #class_prop #index_prop row=row />
        }
    } else if let Type::Path(path) = &field.ty {
        let segment = path.path.segments.last().expect("not empty");
        let type_ident = &segment.ident;

        if type_ident == "FieldGetter" {
            get_default_renderer_for_field_getter(
                &class_prop,
                &value_prop,
                &index_prop,
                segment,
                field,
                &getter,
            )
        } else {
            get_default_renderer_for_type(
                &class_prop,
                &value_prop,
                &index_prop,
                &field.ty,
                field,
                &getter,
            )
        }
    } else {
        panic!("This is not supported")
    }
}

fn get_thead_cell_renderer_for_field(thead_cell_renderer: &Option<IdentString>) -> TokenStream2 {
    if let Some(renderer) = &thead_cell_renderer {
        let ident = renderer.as_ident();
        quote! {#ident}
    } else {
        quote! {leptos_struct_table::DefaultTableHeaderCellRenderer}
    }
}

fn get_getter(name: &Ident, getter: &Option<IdentString>, ty: &Type) -> TokenStream2 {
    match getter {
        Some(getter) => quote! { #getter() },
        None => {
            if let Type::Path(path) = &ty {
                let type_ident = &path.path.segments.last().expect("not empty").ident;
                if type_ident.to_string().as_str() == "FieldGetter" {
                    return quote! { #name() };
                }
            }

            quote! { #name.clone() }
        }
    }
}

fn get_data_provider_logic(
    ident: &TokenStream,
    generic_params: &TokenStream,
    where_clause: &Option<WhereClause>,
    sortable: bool,
    fields: &[&TableRowField],
) -> TokenStream2 {
    let mut column_name_display_arms = vec![];

    let mut column_value_cmp_arms = vec![];

    let mut col_index = 0_usize;

    for f in fields.iter() {
        let name = f.ident.as_ref().expect("named field");
        let TableRowField {
            ref ty,
            skip_sort,
            skip,
            ref getter,
            ..
        } = **f;

        if skip {
            continue;
        }

        let getter = get_getter(name, getter, ty);

        let name_str = name.to_string();

        column_name_display_arms.push(quote! {
            #col_index => #name_str,
        });

        if !skip_sort {
            // TODO : optimize: these getters don't need the clones
            column_value_cmp_arms.push(quote! {
                #col_index => a.#getter.partial_cmp(&b.#getter),
            });
        }

        col_index += 1;
    }

    let cmp_fn = quote! {
        |a: &#ident, b: &#ident, col_index: usize| match col_index {
            #(#column_value_cmp_arms)*
            _ => unreachable!("col_index: {col_index}")
        }
    };

    assert!(
        !column_value_cmp_arms.is_empty(),
        "At least one sortable field is required"
    );

    let set_sorting_impl = if sortable {
        quote! {
            fn set_sorting(&mut self, sorting: &std::collections::VecDeque<(usize, ColumnSort)>) {
                let cmp_fn = #cmp_fn;

                for (col_index, sort) in sorting.iter().rev() {

                    match sort {
                        ColumnSort::Ascending => self.sort_by(|a, b| cmp_fn(a, b, *col_index).unwrap_or(std::cmp::Ordering::Equal)),
                        ColumnSort::Descending => self.sort_by(|a, b| cmp_fn(b, a, *col_index).unwrap_or(std::cmp::Ordering::Equal)),
                        _ => (),
                    }
                }
            }
        }
    } else {
        quote! {}
    };

    quote! {
        impl #generic_params TableDataProvider<#ident> for Vec<#ident>
        #where_clause
        {
            async fn get_rows(&self, range: std::ops::Range<usize> ) -> Result<(Vec<#ident>, std::ops::Range<usize>), String> {
                Ok(leptos_struct_table::get_vec_range_clamped(self, range))
            }

            async fn row_count(&self) -> Option<usize> {
                Some(self.len())
            }

            #set_sorting_impl
        }
    }
}

impl ToTokens for TableRowDeriveInput {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TableRowDeriveInput {
            ref ident,
            ref data,
            ref generics,
            ref thead_cell_renderer,
            ref classes_provider,
            sortable,
            impl_vec_data_provider,
            ref row_type,
            ref i18n,
        } = *self;

        let i18n_path = i18n
            .as_ref()
            .and_then(|i18n| i18n.path.as_ref())
            .map(ToTokens::to_token_stream)
            .unwrap_or_else(|| quote!(crate::i18n));
        let i18n_scope = i18n
            .as_ref()
            .and_then(|i18n| i18n.scope.as_ref())
            .map(ToTokens::to_token_stream);

        let fields = data.as_ref().take_struct().expect("Is not enum").fields;

        let generic_params = &generics.params;
        let where_clause = &generics.where_clause;
        let generic_params_wb = if generic_params.is_empty() {
            quote! {}
        } else {
            quote! {<#generic_params>}
        };

        let ident = row_type.as_ref().map_or(
            quote! { #ident #generic_params_wb },
            |row_type| quote! { #row_type },
        );

        let mut titles = vec![];
        let mut cells = vec![];
        let mut col_name_match_arms = vec![];

        for f in &fields {
            let name = f.ident.as_ref().expect("named field");
            let name_str = name.to_string();

            if f.skip {
                continue;
            }

            let title = if f.skip_header {
                quote! { "" }
            } else if cfg!(feature = "i18n")
                && !f.i18n.as_ref().is_some_and(I18nFieldOptions::is_skipped)
            {
                match f.i18n.as_ref().and_then(|i18n| i18n.key.as_ref()) {
                    Some(key_path) => {
                        quote!({ #i18n_path::t!(_i18n, #key_path) })
                    }
                    None => quote! { { #i18n_path::t!(_i18n, #name) } },
                }
            } else if let Some(ref title) = f.title {
                quote! { #title }
            } else {
                let title = name_str.to_title_case();
                quote! { #title }
            };

            let head_class = f.head_class();

            let thead_cell_renderer = get_thead_cell_renderer_for_field(thead_cell_renderer);

            let index = titles.len();

            let on_click_handling = if sortable && !f.skip_sort {
                quote! { on_click=on_head_click.clone() }
            } else {
                quote! { on_click=|_| () }
            };

            col_name_match_arms.push(quote! {#index => #name_str,});

            titles.push(quote! {
                <#thead_cell_renderer
                    class=leptos::prelude::Signal::derive(move || class_provider.thead_cell(leptos_struct_table::get_sorting_for_column(#index, sorting), #head_class))
                    inner_class=class_provider.thead_cell_inner()
                    index=#index
                    sort_priority=leptos::prelude::Signal::derive(move || {
                        use leptos::prelude::Read;

                        let sorting = sorting.read();
                        if sorting.len() < 2 {
                            return None;
                        }
                        sorting.iter().position(|(index, _)| *index == #index)
                    })
                    sort_direction=leptos::prelude::Signal::derive(move || leptos_struct_table::get_sorting_for_column(#index, sorting))
                    #on_click_handling
                >
                    #title
                </#thead_cell_renderer>
            });

            let cell_renderer = get_renderer_for_field(name, f, cells.len());
            cells.push(cell_renderer);
        }

        let data_provider_logic = if impl_vec_data_provider {
            get_data_provider_logic(&ident, &generic_params_wb, where_clause, sortable, &fields)
        } else {
            quote! {}
        };

        let classes_provider_ident = classes_provider
            .as_ref()
            .map(|id| quote! { #id })
            .unwrap_or(quote! { leptos_struct_table::DummyTableClassesProvider });

        let column_count = cells.len();

        let i18n = if cfg!(feature = "i18n") {
            if let Some(scope) = i18n_scope {
                quote! {
                    let _i18n = {
                        use #i18n_path::use_i18n;
                        #i18n_path::use_i18n_scoped!(#scope)
                    };
                }
            } else {
                quote! {
                    let _i18n = #i18n_path::use_i18n();
                }
            }
        } else {
            quote! {}
        };

        tokens.extend(quote! {
            #data_provider_logic

            impl #generic_params_wb leptos_struct_table::TableRow for #ident
            #where_clause
            {
                type ClassesProvider = #classes_provider_ident;

                const COLUMN_COUNT: usize = #column_count;

                fn render_row(row: leptos::prelude::RwSignal<Self>, index: usize) -> impl leptos::IntoView {
                    use leptos_struct_table::TableClassesProvider;
                    type DefaultMarker = ();

                    let class_provider = Self::ClassesProvider::new();

                    leptos::view! {
                        #(#cells)*
                    }
                }

                fn render_head_row<F>(
                    sorting: leptos::prelude::Signal<std::collections::VecDeque<(usize, leptos_struct_table::ColumnSort)>>,
                    on_head_click: F,
                ) -> impl leptos::IntoView
                where
                    F: Fn(leptos_struct_table::TableHeadEvent) + Clone + 'static,
                {
                    use leptos_struct_table::TableClassesProvider;

                    let class_provider = Self::ClassesProvider::new();

                    #i18n

                    leptos::view! {
                        #(#titles)*
                    }
                }

                fn col_name(col_index: usize) -> &'static str {
                    match col_index {
                        #(#col_name_match_arms)*
                        _ => unreachable!("Column index {} out of bounds", col_index),
                    }
                }
            }
        });
    }
}
