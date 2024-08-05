use crate::models::{TableRowDeriveInput, TableRowField};
use darling::util::IdentString;
use heck::ToTitleCase;
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::__private::TokenStream2;
use syn::{Error, PathSegment, Type, WhereClause};

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
    type_ident: &Ident,
) -> TokenStream {
    let format_props = get_format_props_for_field(field, type_ident);
    match type_ident.to_string().as_str() {
        _ => quote! {
            <leptos_struct_table::DefaultTableCellRenderer options=#format_props #value_prop #class_prop #index_prop on_change=|_| {}/>
        },
    }
}

// TODO: Code duplication with get_field_getter_inner_type --> could be merged in one function
fn get_inner_type<'a, 'b>(
    segment: &'a PathSegment,
    outer_type_name: &'b str,
) -> Result<&'a Ident, syn::Error> {
    let error_message = format!("`{outer_type_name}` should have one type argument");

    if let syn::PathArguments::AngleBracketed(arg) = &segment.arguments {
        if arg.args.len() != 1 {
            return Err(Error::new_spanned(&segment.ident, &error_message));
        }

        let arg = arg.args.first().expect("just checked above");

        if let syn::GenericArgument::Type(Type::Path(path)) = arg {
            Ok(&path.path.segments.last().expect("not empty").ident)
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
    type_ident: &Ident,
    field: &TableRowField,
    getter: &TokenStream2,
) -> TokenStream {
    if let Type::Path(path) = &field.ty {
        let last_segment = path.path.segments.last().expect("not empty");

        return match get_inner_type(last_segment, "Option") {
            Ok(inner_type_ident) => {
                let value_prop = quote! {
                    value=value.clone().expect("not None")
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
                        let value = row.#getter;
                        leptos::view! {
                            <leptos::Show
                                when={
                                    let value = value.clone();
                                    move || value.is_some()
                                }
                                fallback=move || leptos::view!{<leptos_struct_table::DefaultTableCellRenderer value=#none_value.to_string() options={()} #class_prop #index_prop on_change=|_| {}/>}
                            >
                                #inner_renderer
                            </leptos::Show>
                        }
                    }
                }
            }
            Err(err) => err.to_compile_error(),
        };
    }

    Error::new_spanned(type_ident, "Invalid Option type").to_compile_error()
}

fn get_default_renderer_for_type(
    class_prop: &TokenStream,
    value_prop: &TokenStream,
    index_prop: &TokenStream,
    type_ident: &Ident,
    field: &TableRowField,
    getter: &TokenStream2,
) -> TokenStream {
    if type_ident.to_string().starts_with("Option") {
        get_default_option_renderer(class_prop, index_prop, type_ident, field, getter)
    } else {
        get_default_render_for_inner_type(class_prop, value_prop, index_prop, field, type_ident)
    }
}

fn get_format_props_for_field(field: &TableRowField, ty: &Ident) -> TokenStream2 {
    let values: Vec<_> = field
        .format
        .iter()
        .map(|(ident, value)| {
            quote! {o.#ident = Some(#value.into());}
        })
        .collect();

    quote! {
        {
            let mut o = <#ty as ::leptos_struct_table::CellValue>::RenderOptions::default();
            #(#values)*
            o
      }
    }
}

fn get_renderer_for_field(name: &Ident, field: &TableRowField, index: usize) -> TokenStream2 {
    let getter = get_getter(name, &field.getter, &field.ty);

    let index_prop = quote! {
        index=#index
    };

    let class = field.cell_class();
    let class_prop = quote! { class=class_provider.cell( # class) };

    let value_prop = quote! { value=row.#getter };

    let on_change_prop = if is_getter(&field) {
        quote! {on_change=|_| {}}
    } else {
        quote! { on_change={
            let on_change = on_change.clone();
            let row = self.clone();

            move |new_value| {
                let mut changed_row = row.clone();
                changed_row.#name = new_value;

                let event = leptos_struct_table::ChangeEvent {
                    row_index: index,
                    col_index: #index,
                    changed_row,
                };
                on_change.run(event);
            }
        }}
    };

    if let Some(renderer) = &field.renderer {
        let ident = renderer.as_ident();
        quote! {
            <#ident #value_prop #class_prop #index_prop #on_change_prop/>
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
                type_ident,
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

fn is_getter(field: &TableRowField) -> bool {
    if field.getter.is_some() {
        return true;
    }

    if let Type::Path(path) = &field.ty {
        let type_ident = &path.path.segments.last().expect("not empty").ident;
        if type_ident.to_string().as_str() == "FieldGetter" {
            return true;
        }
    }

    false
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
        } = *self;

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
            } else if cfg!(feature = "i18n") {
                quote! { { t!(i18n, #name) } }
            } else if let Some(ref t) = f.title {
                quote! { #t }
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
                    class=leptos::Signal::derive(move || class_provider.thead_cell(leptos_struct_table::get_sorting_for_column(#index, sorting), #head_class))
                    inner_class=class_provider.thead_cell_inner()
                    index=#index
                    sort_priority=leptos::Signal::derive(move || {
                        use leptos::SignalGet;

                        if sorting.get().len() < 2 {
                            return None;
                        }
                        sorting.get().iter().position(|(index, _)| *index == #index)
                    })
                    sort_direction=leptos::Signal::derive(move || leptos_struct_table::get_sorting_for_column(#index, sorting))
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
            quote! {
                let i18n = crate::i18n::use_i18n();
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

                fn render_row(&self, index: usize, on_change: leptos_struct_table::EventHandler<leptos_struct_table::ChangeEvent<Self>>) -> impl leptos::IntoView {
                    use leptos_struct_table::TableClassesProvider;

                    let class_provider = Self::ClassesProvider::new();
                    let row = self.clone();

                    leptos::view! {
                        #(#cells)*
                    }
                }

                fn render_head_row<F>(
                    sorting: leptos::Signal<std::collections::VecDeque<(usize, leptos_struct_table::ColumnSort)>>,
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
