use crate::models::{SelectionMode, TableComponentDeriveInput, TableDataField};
use darling::export::syn::spanned::Spanned;
use darling::util::IdentString;
use heck::{ToTitleCase, ToUpperCamelCase};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::__private::TokenStream2;
use syn::{Error, PathSegment, Type};

fn get_renderer_for_field(name: &syn::Ident, field: &TableDataField, index: usize) -> TokenStream2 {
    let getter = get_getter(name, &field.getter, &field.ty);

    let format_props = get_format_props_for_field(field);

    let index_prop = quote! {
        index=#index
    };

    let class = field.cell_class();
    let class_prop = quote! { class=class_provider.cell( # class) };

    let value_prop = quote! { value=item.#getter };

    if let Some(renderer) = &field.renderer {
        let ident = renderer.as_ident();
        quote! {
            <#ident #format_props #value_prop #class_prop #index_prop/>
        }
    } else if let Type::Path(path) = &field.ty {
        let segment = path.path.segments.last().expect("not empty");
        let type_ident = &segment.ident;

        if type_ident == "FieldGetter" {
            get_default_renderer_for_field_getter(
                &format_props,
                &class_prop,
                &value_prop,
                &index_prop,
                segment,
                field,
                &getter,
            )
        } else {
            get_default_renderer_for_type(
                &format_props,
                &class_prop,
                &value_prop,
                &index_prop,
                type_ident,
                field,
                &getter,
            )
        }
    } else {
        quote! {
            <DefaultTableCellRenderer #format_props #value_prop #class_prop  />
        }
    }
}

fn get_field_getter_inner_type(segment: &PathSegment) -> Result<&Ident, syn::Error> {
    if let syn::PathArguments::AngleBracketed(arg) = &segment.arguments {
        if arg.args.len() != 1 {
            return Err(Error::new_spanned(
                &segment.ident,
                "`FieldGetter` should have one type argument",
            ));
        }

        let arg = arg.args.first().expect("just checked above");

        if let syn::GenericArgument::Type(Type::Path(path)) = arg {
            Ok(&path.path.segments.last().expect("not empty").ident)
        } else {
            Err(Error::new_spanned(
                &segment.ident,
                "`FieldGetter` should have one type argument",
            ))
        }
    } else {
        Err(Error::new_spanned(
            &segment.ident,
            "`FieldGetter` should have one type argument",
        ))
    }
}

fn get_default_renderer_for_field_getter(
    format_props: &TokenStream,
    class_prop: &TokenStream,
    value_prop: &TokenStream,
    index_prop: &TokenStream,
    segment: &PathSegment,
    field: &TableDataField,
    getter: &TokenStream2,
) -> TokenStream {
    match get_field_getter_inner_type(segment) {
        Ok(type_ident) => get_default_renderer_for_type(
            format_props,
            class_prop,
            value_prop,
            index_prop,
            type_ident,
            field,
            getter,
        ),
        Err(err) => err.to_compile_error(),
    }
}

fn get_default_render_for_inner_type(
    format_props: &TokenStream,
    class_prop: &TokenStream,
    value_prop: &TokenStream2,
    index_prop: &TokenStream,
    type_ident: &Ident,
) -> TokenStream {
    match type_ident.to_string().as_str() {
        "NaiveDate" | "NaiveDateTime" | "NaiveTime" => {
            let component_ident = format!("Default{type_ident}TableCellRenderer");
            let component_ident = syn::Ident::new(&component_ident, type_ident.span());

            quote! {
                <#component_ident #format_props #value_prop #class_prop #index_prop/>
            }
        }
        "f32" | "f64" | "Decimal" | "u8" | "u16" | "u32" | "u64" | "u128" | "i8" | "i16"
        | "i32" | "i64" | "i128" => quote! {
            <DefaultNumberTableCellRenderer #format_props #value_prop #class_prop #index_prop/>
        },
        _ => quote! {
            <DefaultTableCellRenderer #format_props #value_prop #class_prop #index_prop/>
        },
    }
}

// TODO: Code duplication with get_field_getter_inner_type --> could be merged in one function
fn get_option_inner_type(segment: &PathSegment) -> Result<&Ident, syn::Error> {
    if let syn::PathArguments::AngleBracketed(arg) = &segment.arguments {
        if arg.args.len() != 1 {
            return Err(Error::new_spanned(
                &segment.ident,
                "`Option` should have one type argument",
            ));
        }

        let arg = arg.args.first().expect("just checked above");

        if let syn::GenericArgument::Type(Type::Path(path)) = arg {
            Ok(&path.path.segments.last().expect("not empty").ident)
        } else {
            Err(Error::new_spanned(
                &segment.ident,
                "`Option` should have one type argument",
            ))
        }
    } else {
        Err(Error::new_spanned(
            &segment.ident,
            "`Option` should have one type argument",
        ))
    }
}

fn get_default_option_renderer(
    format_props: &TokenStream,
    class_prop: &TokenStream,
    index_prop: &TokenStream,
    type_ident: &Ident,
    field: &TableDataField,
    getter: &TokenStream2,
) -> TokenStream {
    if let Type::Path(path) = &field.ty {
        let last_segment = path.path.segments.last().expect("not empty");

        return match get_option_inner_type(last_segment) {
            Ok(inner_type_ident) => {
                let value_prop = quote! {
                    value=item.#getter.expect("not None")
                };

                let none_value = field.none_value.clone().unwrap_or_default();

                let inner_renderer = get_default_render_for_inner_type(
                    format_props,
                    class_prop,
                    &value_prop,
                    index_prop,
                    inner_type_ident,
                );

                quote! {
                    <Show when=move || { item.#getter.is_some() }
                        fallback=move |cx: Scope| view!{cx, <DefaultTableCellRenderer value=#none_value.to_string() #class_prop #index_prop/>}
                    >
                        #inner_renderer
                    </Show>
                }
            }
            Err(err) => err.to_compile_error(),
        };
    }

    Error::new_spanned(type_ident, "Invalid Option type").to_compile_error()
}

fn get_default_renderer_for_type(
    format_props: &TokenStream,
    class_prop: &TokenStream,
    value_prop: &TokenStream,
    index_prop: &TokenStream,
    type_ident: &Ident,
    field: &TableDataField,
    getter: &TokenStream2,
) -> TokenStream {
    if type_ident.to_string().starts_with("Option") {
        get_default_option_renderer(
            format_props,
            class_prop,
            index_prop,
            type_ident,
            field,
            getter,
        )
    } else {
        get_default_render_for_inner_type(
            format_props,
            class_prop,
            value_prop,
            index_prop,
            type_ident,
        )
    }
}

fn get_head_renderer_for_field(head_cell_renderer: &Option<IdentString>) -> TokenStream2 {
    if let Some(renderer) = &head_cell_renderer {
        let ident = renderer.as_ident();
        quote! {#ident}
    } else {
        quote! {DefaultTableHeaderRenderer}
    }
}

fn get_format_props_for_field(field: &TableDataField) -> TokenStream2 {
    let precision = if let Some(p) = &field.format.precision {
        quote! {precision=(#p as usize)}
    } else {
        quote! {}
    };

    let format_string = if let Some(f) = &field.format.string {
        quote! {format_string=#f.to_string()}
    } else {
        quote! {}
    };

    let getter = get_getter(name, &field.getter, &field.ty);

    let on_cell_change = {
        if let None = &field.getter {
            if let Type::Path(path) = &field.ty {
                let type_ident = &path.path.segments.last().expect("not empty").ident;
                if type_ident.to_string().as_str() == "FieldGetter" {
                    return quote! { on_change=move |_| ()};
                }
            }
        }
        quote! {
            on_change=move |v| {
                local_items_state.update_untracked(move |items| {
                    if let Some(mut value) = items.as_mut().map(|s| s.get_mut(i)).flatten() {
                        value.#name = v;
                        action_set_row.dispatch((i, value.clone()));
                    }
                });
            }
        }
    };

    let editable = if field.editable {
        quote! { true }
    } else {
        quote! { false }
    };

    quote! {
        value=item.#getter
        class=class_provider.cell(#class)
        #on_cell_change
        editable=#editable
        #precision
        #format_string
    }
}

fn get_selection_logic(
    selection_mode: &SelectionMode,
    key_type: &Type,
) -> (TokenStream2, TokenStream2, TokenStream2) {
    match selection_mode {
        SelectionMode::None => (quote! {}, quote! {}, quote! {|_| false}),
        SelectionMode::Single => (
            quote! {
                selected_key.update(|selected_key| { *selected_key = Some(event.key); });
            },
            quote! {
                /// Generated by leptos-struct-table-macro
                selected_key: RwSignal<Option<#key_type>>,
            },
            quote! {create_selector(cx, selected_key)},
        ),
        SelectionMode::Multiple => unimplemented!("Multiple selection not implemented yet"),
    }
}

fn get_getter(name: &syn::Ident, getter: &Option<IdentString>, ty: &Type) -> TokenStream2 {
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
    ident: &syn::Ident,
    sortable: bool,
    fields: &[&TableDataField],
    column_name_enum: &syn::Ident,
) -> TokenStream2 {
    let column_value_enum = &format_ident!("{}ColumnValue", ident);

    let mut column_name_variants = vec![];
    let mut column_value_variants = vec![];

    let mut column_value_cmp_arms = vec![];
    let mut column_value_get_arms = vec![];

    for f in fields.iter() {
        let name = f.ident.as_ref().expect("named field");
        let TableDataField {
            ref ty,
            skip_sort,
            skip,
            ref getter,
            ..
        } = **f;

        if (skip_sort || skip) && !f.key {
            continue;
        }

        let getter = get_getter(name, getter, ty);

        let column_name_variant =
            syn::Ident::new(&name.to_string().to_upper_camel_case(), name.span());

        column_name_variants.push(if f.key {
            quote! {
                /// Generated by leptos-struct-table-macro
                #[default]
                #column_name_variant,
            }
        } else {
            quote! {
                /// Generated by leptos-struct-table-macro
                #column_name_variant,
            }
        });

        if let Type::Path(path) = &ty {
            let segment = path.path.segments.last().expect("not empty");
            let type_ident = &segment.ident;

            if type_ident == "FieldGetter" {
                match get_field_getter_inner_type(segment) {
                    Ok(type_ident) => {
                        column_value_variants.push(quote! {
                            /// Generated by leptos-struct-table-macro
                            #column_name_variant(#type_ident),
                        });
                    }
                    Err(err) => {
                        return err.to_compile_error();
                    }
                }
            } else {
                column_value_variants.push(quote! {
                    /// Generated by leptos-struct-table-macro
                    #column_name_variant(#ty),
                });
            }
        } else {
            column_value_variants.push(quote! {
                /// Generated by leptos-struct-table-macro
                #column_name_variant(#ty),
            });
        }

        column_value_cmp_arms.push(quote! {
            (#column_value_enum::#column_name_variant(a), #column_value_enum::#column_name_variant(b)) => a.partial_cmp(b),
        });

        column_value_get_arms.push(quote! {
            #column_name_enum::#column_name_variant => #column_value_enum::#column_name_variant(self.#getter),
        });
    }

    assert!(
        !column_name_variants.is_empty(),
        "At least one sortable field is required"
    );

    let (partial_ord_impl, set_sorting_impl) = if sortable {
        (
            quote! {
                match (self, other) {
                    #(#column_value_cmp_arms)*
                    _ => unreachable!()
                }
            },
            quote! {
                fn set_sorting(&mut self, sorting: &std::collections::VecDeque<(Self::ColumnName, ColumnSort)>) {
                    for (field, sort) in sorting.iter().rev() {
                        match sort {
                            ColumnSort::Ascending => self.sort_by(|a, b| a.get(*field).partial_cmp(&b.get(*field)).unwrap_or(std::cmp::Ordering::Equal)),
                            ColumnSort::Descending => self.sort_by(|a, b| b.get(*field).partial_cmp(&a.get(*field)).unwrap_or(std::cmp::Ordering::Equal)),
                            _ => (),
                        }
                    }
                }
            },
        )
    } else {
        (quote! { None }, quote! {})
    };

    quote! {
        /// Generated by leptos-struct-table-macro
        #[derive(Copy, Clone, PartialEq, Eq, Debug, Default, Deserialize, Serialize)]
        pub enum #column_name_enum {
            #(#column_name_variants)*
        }

        /// Generated by leptos-struct-table-macro
        #[derive(PartialEq)]
        pub enum #column_value_enum {
            #(#column_value_variants)*
        }

        impl PartialOrd for #column_value_enum {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                #partial_ord_impl
            }
        }

        impl #ident {
            pub fn get(&self, column: #column_name_enum) -> #column_value_enum {
                match column {
                    #(#column_value_get_arms)*
                    _ => unreachable!()
                }
            }
        }

        impl TableDataSorting<#ident> for Vec<#ident> {
            type ColumnName = #column_name_enum;

            #set_sorting_impl
        }
    }
}

impl ToTokens for TableComponentDeriveInput {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TableComponentDeriveInput {
            ref ident,
            ref data,
            ref component_name,
            ref classes_provider,
            ref head_row_renderer,
            ref row_renderer,
            ref tag,
            ref row_class,
            ref head_row_class,
            ref head_cell_renderer,
            ref selection_mode,
            ref thead_renderer,
            ref tbody_renderer,
            sortable,
        } = *self;

        let mut key_field_and_type = None;

        let row_renderer = row_renderer
            .as_ref()
            .map(|r| r.as_ident().clone())
            .unwrap_or_else(|| syn::Ident::new("DefaultTableRowRenderer", row_renderer.span()));

        let head_row_renderer = head_row_renderer
            .as_ref()
            .map(|r| r.as_ident().clone())
            .unwrap_or_else(|| syn::Ident::new("tr", head_row_renderer.span()));

        let thead_renderer = thead_renderer
            .as_ref()
            .map(|r| r.as_ident().clone())
            .unwrap_or_else(|| syn::Ident::new("thead", thead_renderer.span()));

        let tbody_renderer = tbody_renderer
            .as_ref()
            .map(|r| r.as_ident().clone())
            .unwrap_or_else(|| syn::Ident::new("tbody", tbody_renderer.span()));

        let tag = tag
            .as_ref()
            .map(|s| {
                let t = s.as_ident();
                quote!(#t)
            })
            .unwrap_or(quote!(table));

        let row_class = row_class.as_ref().cloned().unwrap_or("".to_owned());

        let head_row_class = head_row_class.as_ref().cloned().unwrap_or("".to_owned());

        let fields = data.as_ref().take_struct().expect("Is not enum").fields;

        let column_name_enum = &format_ident!("{}ColumnName", ident);

        let data_provider_logic =
            get_data_provider_logic(ident, sortable, &fields, column_name_enum);

        let mut titles = vec![];
        let mut cells = vec![];

        for f in fields.into_iter() {
            let name = f.ident.as_ref().expect("named field");

            if f.key {
                if key_field_and_type.is_some() {
                    tokens.extend(
                        Error::new_spanned(&f.ident, "Only one field can be marked as key")
                            .to_compile_error(),
                    );
                    return;
                }
                if f.editable {
                    tokens.extend(
                        Error::new_spanned(&f.ident, "A field marked as key cannot be editable.")
                            .to_compile_error(),
                    );
                    return;
                }
                key_field_and_type = Some((name.clone(), f.ty.clone()));
            }

            if f.skip {
                continue;
            }

            let title = if let Some(ref t) = f.title {
                t.clone()
            } else {
                name.to_string().to_title_case()
            };

            let head_class = f.head_class();

            let head_renderer = get_head_renderer_for_field(head_cell_renderer);

            let index = titles.len();
            let column_name_variant =
                syn::Ident::new(&name.to_string().to_upper_camel_case(), name.span());

            titles.push(quote! {
                <#head_renderer
                    class=Signal::derive(cx, move || class_provider.clone().head_cell(column_sort.clone()(#column_name_enum::#column_name_variant), #head_class))
                    inner_class=class_provider.clone().head_cell_inner()
                    index=#index
                    column=#column_name_enum::#column_name_variant
                    sort_priority=Signal::derive(cx, move || {
                        if sorting().len() < 2 {
                            return None;
                        }
                        sorting().iter().position(|(field, _)| *field == #column_name_enum::#column_name_variant)
                    })
                    sort_direction=Signal::derive(cx, move || column_sort.clone()(#column_name_enum::#column_name_variant))
                    on_click=on_head_click.clone()
                >
                    #title
                </#head_renderer>
            });

            let cell_renderer = get_renderer_for_field(name, f, cells.len());
            cells.push(cell_renderer);
        }

        if key_field_and_type.is_none() {
            // TODO : how to get the span of the fields?
            tokens.extend(
                Error::new_spanned(ident, "One field must be marked as #[table(key)]")
                    .to_compile_error(),
            );
            return;
        }

        let (key_field, key_type) = key_field_and_type.unwrap();

        let default_component_ident_name = format!("{ident}Table");
        let component_ident = component_name
            .as_ref()
            .unwrap_or(&default_component_ident_name);
        let component_ident = syn::Ident::new(component_ident, ident.span());

        let classes_provider_ident = classes_provider
            .as_ref()
            .map(|id| id.to_string())
            .unwrap_or(format!("{}ClassesProvider", component_ident));
        let classes_provider_ident =
            syn::Ident::new(&classes_provider_ident, classes_provider.span());

        let default_classes_provider_def = match classes_provider {
            None => quote! {
                /// Generated by leptos-struct-table-macro
                #[derive(Copy, Clone)]
                struct #classes_provider_ident;

                impl TableClassesProvider for #classes_provider_ident {
                    fn new() -> Self {
                        Self
                    }
                }
            },
            Some(_) => quote! {},
        };

        let (selection_handler, selection_prop, selector) =
            get_selection_logic(selection_mode, &key_type);

        tokens.extend(quote! {
            // TODO : pagination

            #default_classes_provider_def

            #data_provider_logic

            #[allow(non_snake_case)]
            #[component]
            pub fn #component_ident<D>(
                cx: Scope,
                /// Class name of the HTML tag.
                #[prop(optional)] class: String,
                /// Data storage provider.
                data_provider: StoredValue<D>,
                /// Optional range of the data to be shown.
                #[prop(optional)] range: Option<RwSignal<core::ops::Range<usize>>>,
                // #[prop(optional)] on_row_click: Option<FR>,
                #selection_prop
                // #[prop(optional)] on_head_click: Option<FH>,
            ) -> impl IntoView
            where
                D: TableDataStorage<#ident> + 'static,
                //T: TableDataProvider<#ident, ColumnName = #column_name_enum> + Clone + PartialEq + core::fmt::Debug + 'static,
                // FR: Fn(TableRowEvent<#key_type>) + Clone + 'static,
                // FH: Fn(FieldValue) + 'static,
            {
                let class_provider = #classes_provider_ident::new();

                let (range, set_range) = if let Some(r) = range {
                    r.split()
                } else {
                    create_signal(cx, 0..1000)
                };

                let on_row_select = move |event: TableRowEvent<#key_type>| {
                    #selection_handler
                    // on_row_click(event);
                };

                let (sorting, set_sorting) = create_signal(cx, std::collections::VecDeque::<(#column_name_enum, ColumnSort)>::new());

                let on_head_click = move |event: TableHeadEvent<#column_name_enum>| {
                    set_sorting.update(move |sorting| {
                        let (i, (_, mut sort)) = sorting.iter().enumerate().find(|(_, (column, _))| column == &event.column).unwrap_or((0, &(event.column, ColumnSort::None)));

                        if i == 0 || sort == ColumnSort::None {
                            sort = match sort {
                                ColumnSort::None => ColumnSort::Ascending,
                                ColumnSort::Ascending => ColumnSort::Descending,
                                ColumnSort::Descending => ColumnSort::None,
                            };
                        }

                        *sorting = sorting.clone()
                                        .into_iter()
                                        .filter(|(column, sort)| *column != event.column && *sort != ColumnSort::None)
                                        .collect();

                        if sort != ColumnSort::None {
                            sorting.push_front((event.column, sort));
                        }
                    });
                };

                let action_set_row = create_action(cx, move |(idx, row): &(usize, #ident)| {
                    let mut provider = data_provider.get_value();
                    let row = row.clone();
                    let idx = idx.clone();
                    async move {
                        provider.set_row(idx, row).await
                    }
                });

                let sort = sorting.clone();

                let column_sort = move |name_variant: #column_name_enum| {
                    sort().into_iter()
                        .find(|(field, _)| *field == name_variant)
                        .map(|(_, sort)| sort)
                        .unwrap_or(ColumnSort::None)
                };

                let local_items_state = create_rw_signal(cx, None::<Vec<#ident>>);
                let fetched_items = create_local_resource(cx,
                    move || range.get(),
                    move |range| {
                        let provider = data_provider.get_value();
                        async move {
                            let rows = match provider.get_rows(range.clone()).await {
                                Ok(r) => r,
                                Err(e) => {
                                    log::error!("Could not get rows: {e}");
                                    return vec![];
                                },
                            };
                            rows
                        }
                    }
                );

                let memo_update_local_items_state = move || {
                    local_items_state.set(fetched_items.read(cx));
                };

                let memo_items = create_memo(cx, move |_| {
                    let sort = sorting.get();
                    local_items_state.with(|items| {
                        if let Some(it) = items {
                            let mut sorted_items = it.clone();
                            sorted_items.set_sorting(&sort);
                            Some(sorted_items)
                        } else {
                            None
                        }
                    })
                });

                view! { cx,
                    { memo_update_local_items_state }
                    <#tag class=class_provider.table(&class)>
                        <#thead_renderer>
                            <#head_row_renderer class=class_provider.head_row(#head_row_class)>
                                #(#titles)*
                            </#head_row_renderer>
                        </#thead_renderer>

                        <#tbody_renderer>
                        <Transition fallback=move || view! {cx, <tr><td colspan="4">"Loading...!"</td></tr> }>
                            { move || {
                                let is_selected = #selector;
                                let items = memo_items.get();
                                items.map(move |items| {
                                    let items = items.into_iter().enumerate().collect::<Vec<_>>();
                                view! { cx,
                                        <For
                                            each=move || items.clone()
                                            key=|(_, item)| item.#key_field.clone()
                                            view=move |cx, (i, item)| {
                                                let is_sel = is_selected.clone();

                                                let class_signal = Signal::derive(
                                                    cx,
                                                    move || class_provider.clone().row(i, is_sel(Some(item.#key_field.clone())), #row_class),
                                                );

                                                let is_sel = is_selected.clone();

                                                let selected_signal = Signal::derive(cx, move || is_sel(Some(item.#key_field.clone())));

                                                view! { cx,
                                                    <#row_renderer
                                                        class=class_signal
                                                        key=item.#key_field.clone()
                                                        index=i
                                                        selected=selected_signal
                                                        on_click=on_row_select
                                                    >
                                                        #(#cells)*
                                                    </#row_renderer>
                                                }
                                            }
                                        />
                                    }
                                })
                            } }
                        </Transition>
                        </#tbody_renderer>
                    </#tag>
                }
            }
        });
    }
}
