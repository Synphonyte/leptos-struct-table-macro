use crate::models::{ColumnIndexType, I18nFieldOptions, TableRowDeriveInput, TableRowField};
use darling::util::IdentString;
use heck::{ToTitleCase, ToUpperCamelCase};
use proc_macro2::{Ident, TokenStream};
use quote::{ToTokens, format_ident, quote};
use syn::{__private::TokenStream2, Error, PathSegment, Type, WhereClause};

fn get_default_renderer_for_field_getter(
    class_prop: &TokenStream,
    value_prop: &TokenStream,
    index_prop: &TokenStream,
    column_type: &TokenStream,
    segment: &PathSegment,
    field: &TableRowField,
    getter: &TokenStream2,
) -> TokenStream {
    match get_inner_type(segment, "FieldGetter") {
        Ok(type_ident) => get_default_renderer_for_type(
            class_prop,
            value_prop,
            index_prop,
            column_type,
            type_ident,
            field,
            getter,
        ),
        Err(err) => err.to_compile_error(),
    }
}

fn get_default_render_for_inner_type(
    class_prop: &TokenStream,
    value_prop: &TokenStream2,
    index_prop: &TokenStream,
    column_type: &TokenStream,
    field: &TableRowField,
    type_ident: &syn::Type,
) -> TokenStream {
    let format_props = get_format_props_for_field(field, type_ident);
    let marker = field.marker.as_ref().map_or_else(
        || get_default_cell_value_marker(type_ident),
        |marker| quote! { #marker },
    );

    quote! {
        <leptos_struct_table::DefaultTableCellRenderer<_, #column_type, #type_ident, #marker> options=#format_props #value_prop #class_prop #index_prop row=row />
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
    column_type: &TokenStream,
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
                    column_type,
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
                                        <leptos_struct_table::DefaultTableCellRenderer<_, #column_type, String, DefaultMarker>
                                            value=leptos::prelude::Signal::stored(#none_value.to_string())
                                            options=()
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
    column_type: &TokenStream,
    type_ident: &syn::Type,
    field: &TableRowField,
    getter: &TokenStream2,
) -> TokenStream {
    if is_option(type_ident) {
        get_default_option_renderer(
            class_prop,
            index_prop,
            type_ident,
            column_type,
            field,
            getter,
        )
    } else {
        get_default_render_for_inner_type(
            class_prop,
            value_prop,
            index_prop,
            column_type,
            field,
            type_ident,
        )
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

/// * index: contains the value for the generic column type.
fn get_renderer_for_field(
    name: &Ident,
    field: &TableRowField,
    column_type: &TokenStream,
    index: &TokenStream,
    extra_classes: Vec<TokenStream>,
) -> TokenStream2 {
    let getter = get_getter(name, &field.getter, &field.ty);

    let index_prop = quote! {
        index=#index
    };

    let class = field.cell_class();
    let joined_classes = if extra_classes.is_empty() {
        quote! { #class }
    } else {
        let mut places = extra_classes.iter().map(|_| "{}").collect::<Vec<&str>>();
        places.push("{}");
        let places = places.join(" ");
        quote! { format!(#places, #class #(, #extra_classes)*).as_str() }
    };
    let class_prop = quote! { class=leptos_struct_table::TableClassesProvider::cell(&class_provider, #joined_classes) };

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
                column_type,
                segment,
                field,
                &getter,
            )
        } else {
            get_default_renderer_for_type(
                &class_prop,
                &value_prop,
                &index_prop,
                column_type,
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
    column_type: &TokenStream,
    column_variants: impl IntoIterator<Item = TokenStream>,
    where_clause: &Option<WhereClause>,
    sortable: bool,
    fields: &[&TableRowField],
) -> TokenStream2 {
    let mut column_name_display_arms = vec![];

    let mut column_value_cmp_arms = vec![];

    let mut column_variants = column_variants.into_iter();

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

        let col_variant = column_variants.next().expect(
            "there need to be enough variants for each column (internal invariant broken).",
        );

        column_name_display_arms.push(quote! {
            #col_variant => #name_str,
        });

        if !skip_sort {
            // TODO : optimize: these getters don't need the clones
            column_value_cmp_arms.push(quote! {
                #col_variant => a.#getter.partial_cmp(&b.#getter),
            });
        }
    }

    let cmp_fn = quote! {
        |a: &#ident, b: &#ident, col_index: #column_type| match col_index {
            #(#column_value_cmp_arms)*
            _ => unreachable!("col_index: {col_index:?}")
        }
    };

    assert!(
        !column_value_cmp_arms.is_empty(),
        "At least one sortable field is required"
    );

    let set_sorting_impl = if sortable {
        quote! {
            fn set_sorting(&mut self, sorting: &std::collections::VecDeque<(#column_type, ColumnSort)>) {
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
        impl #generic_params TableDataProvider<#ident, #column_type> for Vec<#ident>
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
            ref vis,
            ref data,
            ref generics,
            ref thead_cell_renderer,
            ref classes_provider,
            sortable,
            impl_vec_data_provider,
            ref column_index_type,
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

        let table_row_name = ident;
        let ident = row_type.as_ref().map_or(
            quote! { #ident #generic_params_wb },
            |row_type| quote! { #row_type },
        );

        let classes_provider_ident = classes_provider
            .as_ref()
            .map(|id| quote! { #id })
            .unwrap_or(quote! { leptos_struct_table::DummyTableClassesProvider });

        // Head-row cells
        let mut titles = vec![];
        // Row cells
        let mut cells = vec![];
        // Row cell functions, e.g. fun cell_renderer_for_field_name() -> impl IntoView { ... }
        let mut cell_funs = vec![];
        // Match arms on column type to call the appropriate cell renderer for that column.
        let mut cell_renderer_invocation_match_arms = vec![];
        // List of Column => "name",
        let mut col_name_match_arms = vec![];

        // Accumulates the enum variants during the fields loop
        let mut enum_tokens: Option<_> = if matches!(column_index_type, ColumnIndexType::Enum) {
            Some(quote! {})
        } else {
            None
        };

        let column_type_vis = vis;
        let column_type_name = format_ident!("{table_row_name}Column");
        let column_type = match column_index_type {
            ColumnIndexType::Usize => quote! { usize },
            ColumnIndexType::Enum => quote! { #column_type_name },
        };

        // Fully qualified columng_type variants
        let mut column_variants: Vec<TokenStream> = Vec::new();

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

            let cell_index = titles.len();

            let on_click_handling = if sortable && !f.skip_sort {
                quote! { on_click=on_head_click.clone() }
            } else {
                quote! { on_click=|_| () }
            };

            let variant_name = format_ident!("{}", name_str.to_upper_camel_case());
            let column = match column_index_type {
                ColumnIndexType::Usize => quote! { #cell_index },
                ColumnIndexType::Enum => quote! { #column_type_name::#variant_name },
            };
            column_variants.push(quote! { #column });
            col_name_match_arms.push(quote! {#column => #name_str,});

            if let Some(enum_variants) = &mut enum_tokens {
                let doc = format!("Generated variant for [{name_str}]");
                enum_variants.extend(quote! {
                #[doc = #doc]
                ///
                #variant_name, });
            }

            let thead_renderer = quote! {
                <#thead_cell_renderer
                    class=leptos::prelude::Signal::derive(move || class_provider.thead_cell(leptos_struct_table::get_sorting_for_column(#column, sorting), #head_class))
                    inner_class=class_provider.thead_cell_inner()
                    index=#column
                    sort_priority=leptos::prelude::Signal::derive(move || {
                        use leptos::prelude::Read;

                        let sorting = sorting.read();
                        if sorting.len() < 2 {
                            return None;
                        }
                        sorting.iter().position(|(index, _)| *index == #column)
                    })
                    sort_direction=leptos::prelude::Signal::derive(move || leptos_struct_table::get_sorting_for_column(#column, sorting))
                    #on_click_handling
                    drag_state=drag_state
                    drag_handler=drag_handler.clone()
                    columns=columns
                >
                    #title
                </#thead_cell_renderer>
            };
            titles.push(quote! { #column => leptos::view! { #thead_renderer }.into_any(), });

            // Exposing default cell renderer logic for cell_renderer_for_column.
            // there is some added complexity due to needing to pass classes into Fn closures.
            let cell_renderer_dyn_class = get_renderer_for_field(
                name,
                f,
                &column_type,
                &column,
                vec![quote! { leptos::prelude::ReadValue::read_value(&dynamic_class) }],
            );
            let cell_renderer_fn_ident = format_ident!("cell_renderer_for_{name}");
            cell_funs.push(quote! {
                fn #cell_renderer_fn_ident(row: RwSignal<#ident>, dynamic_class: String) -> leptos::prelude::AnyView {
                    type DefaultMarker = ();
                    let dynamic_class = leptos::prelude::StoredValue::new(dynamic_class); // Needed a Copy type to pass into Fn or FnMut closures.

                    let class_provider = <#classes_provider_ident as leptos_struct_table::TableClassesProvider>::new();

                    leptos::prelude::IntoAny::into_any(leptos::view! { #cell_renderer_dyn_class })
                }
            });
            cell_renderer_invocation_match_arms
                .push(quote! { #column => Self::#cell_renderer_fn_ident(row, dynamic_class), });

            // Normal cell renderers used in render_row
            let cell_renderer = get_renderer_for_field(name, f, &column_type, &column, vec![]);
            cells.push(quote! { #column => leptos::prelude::IntoAny::into_any(leptos::view! { #cell_renderer }), });
        }

        if let Some(enum_variants) = enum_tokens {
            enum_tokens = Some(quote! {
                /// Generated enum, variants are the unskipped fields of the associated struct.
                #[derive(Hash, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Debug)]
                #column_type_vis enum #column_type_name {
                    #enum_variants
                }
            });
        }

        let data_provider_logic = if impl_vec_data_provider {
            get_data_provider_logic(
                &ident,
                &generic_params_wb,
                &column_type,
                column_variants.clone(),
                where_clause,
                sortable,
                &fields,
            )
        } else {
            quote! {}
        };

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
            use leptos::prelude::{Signal, RwSignal};

            #data_provider_logic

            #enum_tokens

            impl #generic_params_wb #ident
            #where_clause
            {
                #(#cell_funs)*
            }

            impl #generic_params_wb leptos_struct_table::TableRow<#column_type> for #ident
            #where_clause
            {
                type ClassesProvider = #classes_provider_ident;

                const COLUMN_COUNT: usize = #column_count;

                fn render_row(row: RwSignal<Self>, index: usize, columns: RwSignal<Vec<#column_type>>) -> impl leptos::IntoView {
                    use leptos_struct_table::TableClassesProvider;
                    use leptos::control_flow::For;
                    use leptos::prelude::{Get, IntoAny};
                    type DefaultMarker = ();

                    let class_provider = Self::ClassesProvider::new();
                    leptos::view! {
                        <For
                            each=move || columns.get().into_iter()
                            key=|column| column.clone()
                            children=move |column| {
                                match column {
                                    #(#cells)*
                                    other => leptos::view! {
                                        format!("Unmatched column type {other:?}, probably a bug.") 
                                    }.into_any()
                                }
                            }>
                        </For>
                    }
                }

                fn render_head_row<F>(
                    sorting: Signal<std::collections::VecDeque<(#column_type, leptos_struct_table::ColumnSort)>>,
                    on_head_click: F,
                    drag_handler: leptos_struct_table::HeadDragHandler<#column_type>,
                    columns: RwSignal<Vec<#column_type>>
                ) -> impl leptos::IntoView
                where
                    F: Fn(leptos_struct_table::TableHeadEvent<#column_type>) + Send + Clone + 'static,
                {
                    use leptos_struct_table::TableClassesProvider;
                    use leptos::prelude::{Get, IntoAny};
                    use leptos::control_flow::For;

                    let class_provider = Self::ClassesProvider::new();

                    #i18n

                    let drag_state = leptos_struct_table::DragStateRwSignal::new(None);

                    leptos::view! {
                        <For
                            each=move || columns.get().into_iter()
                            key=|column| column.clone()
                            children=move |column| {
                                match column {
                                    #(#titles)*
                                    other => leptos::view! {
                                        format!("Unmatched column type {other:?}, probably a bug.") 
                                    }.into_any()
                                }
                            }>
                        </For>
                    }
                }

                /// Returns the cell renderer for a **column**
                /// Allows to create custom row renders while still using the annotation configured cell renderers.
                fn cell_renderer_for_column(row: RwSignal<#ident>, column: #column_type, dynamic_class: String) -> leptos::prelude::AnyView {
                    match column {
                        #(#cell_renderer_invocation_match_arms)*
                        other => leptos::prelude::IntoAny::into_any(leptos::view! {
                            format!("Unmatched column type {other:?}, probably a bug.") 
                        })
                    }
                }

                fn columns() -> &'static [#column_type] {
                    return &[#(#column_variants,)*]
                }

                fn col_name(col_index: #column_type) -> &'static str {
                    match col_index {
                        #(#col_name_match_arms)*
                        _ => unreachable!("Column index {:?} out of bounds", col_index),
                    }
                }
            }
        });
    }
}
