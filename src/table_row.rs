use crate::models::{TableDataField, TableRowDeriveInput};
use darling::util::IdentString;
use heck::ToTitleCase;
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::__private::TokenStream2;
use syn::spanned::Spanned;
use syn::{Error, PathSegment, Type};

fn get_default_renderer_for_field_getter(
    format_props: &TokenStream,
    class_prop: &TokenStream,
    value_prop: &TokenStream,
    index_prop: &TokenStream,
    segment: &PathSegment,
    field: &TableDataField,
    getter: &TokenStream2,
) -> TokenStream {
    match get_inner_type(segment, "FieldGetter") {
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
                <#component_ident #format_props #value_prop #class_prop #index_prop on_change=|_| {}/>
            }
        }
        "f32" | "f64" | "Decimal" | "u8" | "u16" | "u32" | "u64" | "u128" | "i8" | "i16"
        | "i32" | "i64" | "i128" => quote! {
            <DefaultNumberTableCellRenderer #format_props #value_prop #class_prop #index_prop on_change=|_| {}/>
        },
        _ => quote! {
            <DefaultTableCellRenderer #format_props #value_prop #class_prop #index_prop on_change=|_| {}/>
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
    format_props: &TokenStream,
    class_prop: &TokenStream,
    index_prop: &TokenStream,
    type_ident: &Ident,
    field: &TableDataField,
    getter: &TokenStream2,
) -> TokenStream {
    if let Type::Path(path) = &field.ty {
        let last_segment = path.path.segments.last().expect("not empty");

        return match get_inner_type(last_segment, "Option") {
            Ok(inner_type_ident) => {
                let value_prop = quote! {
                    value=row.#getter.expect("not None")
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
                    <Show when={
                        let value = row.#getter;
                        move || { value.is_some() }
                    }
                        fallback=move || view!{<DefaultTableCellRenderer value=#none_value.to_string() #class_prop #index_prop on_change=|_| {}/>}
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

    quote! {
        #precision
        #format_string
    }
}

fn get_renderer_for_field(name: &Ident, field: &TableDataField, index: usize) -> TokenStream2 {
    let getter = get_getter(name, &field.getter, &field.ty);

    let format_props = get_format_props_for_field(field);

    let index_prop = quote! {
        index=#index
    };

    let class = field.cell_class();
    let class_prop = quote! { class=class_provider.cell( # class) };

    let value_prop = quote! { value=row.#getter };

    let on_change_prop = quote! { on_change=move |new_value| {
        on_change.with_value(|on_change| {
            let mut changed_row = row.clone();
            changed_row.#getter = new_value;

            let event = TableChangeEvent {
                row_index: i,
                col_index: #index,
                changed_row,
            };
            on_change.call(event);
        });
    } };

    if let Some(renderer) = &field.renderer {
        let ident = renderer.as_ident();
        quote! {
            <#ident #format_props #value_prop #class_prop #index_prop #on_change_prop/>
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
            <DefaultTableCellRenderer #format_props #value_prop #class_prop on_change=|_| {} />
        }
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
    generics: &syn::Generics,
    sortable: bool,
    fields: &[&TableDataField],
) -> TokenStream2 {
    let mut column_name_display_arms = vec![];

    let mut column_value_cmp_arms = vec![];

    for (col_index, f) in fields.iter().enumerate() {
        let name = f.ident.as_ref().expect("named field");
        let TableDataField {
            ref ty,
            skip_sort,
            skip,
            ref getter,
            ..
        } = **f;

        if (skip) && !f.key {
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
    }

    let cmp_fn = quote! {
        |a: &#ident, b: &#ident, col_index: usize| match col_index {
            #(#column_value_cmp_arms)*
            _ => unreachable!()
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

    let generic_params = if generics.params.is_empty() {
        quote! {}
    } else {
        let params = &generics.params;
        quote! {<#params>}
    };
    let where_clause = &generics.where_clause;

    quote! {
        #[async_trait(?Send)]
        impl #generic_params TableDataProvider<#ident #generic_params> for Vec<#ident #generic_params>
        #where_clause
        {
            async fn get_rows(&self, range: std::ops::Range<usize> ) -> Result<(Vec<#ident #generic_params>, std::ops::Range<usize>), String> {
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
            ref head_cell_renderer,
            ref selection_mode,
            ref classes_provider,
            sortable,
            impl_vec_data_provider,
        } = *self;

        let mut key_field_and_type = None;

        let fields = data.as_ref().take_struct().expect("Is not enum").fields;

        let mut titles = vec![];
        let mut cells = vec![];

        for f in &fields {
            let name = f.ident.as_ref().expect("named field");

            if f.key {
                if key_field_and_type.is_some() {
                    tokens.extend(
                        Error::new_spanned(&f.ident, "Only one field can be marked as key")
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

            let on_click_handling = if sortable && !f.skip_sort {
                quote! { on_click=on_head_click.clone() }
            } else {
                quote! { on_click=|_| () }
            };

            titles.push(quote! {
                <#head_renderer
                    class=Signal::derive(move || class_provider.thead_cell(get_sorting_for_column(#index, sorting), #head_class))
                    inner_class=class_provider.thead_cell_inner()
                    index=#index
                    sort_priority=Signal::derive(move || {
                        if sorting.get().len() < 2 {
                            return None;
                        }
                        sorting.get().iter().position(|(index, _)| *index == #index)
                    })
                    sort_direction=Signal::derive(move || get_sorting_for_column(#index, sorting))
                    #on_click_handling
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

        let data_provider_logic = if impl_vec_data_provider {
            get_data_provider_logic(ident, generics, sortable, &fields)
        } else {
            quote! {}
        };

        let generic_params = &generics.params;
        let where_predicates = generics.where_clause.as_ref().map(|w| w.predicates.clone());
        let generic_params_wb = if generic_params.is_empty() {
            quote! {}
        } else {
            quote! {<#generic_params>}
        };

        let classes_provider_ident = classes_provider
            .as_ref()
            .map(|id| id.to_string())
            .unwrap_or("DummyTableClassesProvider".to_string());
        let classes_provider_ident =
            syn::Ident::new(&classes_provider_ident, classes_provider.span());

        tokens.extend(quote! {
            #data_provider_logic

            impl #generic_params_wb RowRenderer for #ident #generic_params_wb
            #where_predicates
            {
                type ClassesProvider = #classes_provider_ident;

                fn key(&self) -> String {
                    self.#key_field.to_string()
                }

                fn render_row(&self) -> impl IntoView
                {
                    let class_provider = Self::ClassesProvider::new();
                    let row = self.clone();

                    view! {
                        #(#cells)*
                    }
                }

                fn render_head_row<F>(
                    sorting: Signal<std::collections::VecDeque<(usize, ColumnSort)>>,
                    on_head_click: F,
                ) -> impl IntoView
                where
                    F: Fn(TableHeadEvent) + Clone + 'static,
                {
                    let class_provider = Self::ClassesProvider::new();

                    view! {
                        #(#titles)*
                    }
                }
            }
        });
    }
}
