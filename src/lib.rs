mod models;
mod table_row;

use darling::FromDeriveInput;
use models::TableRowDeriveInput;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_derive(TableRow, attributes(table))]
pub fn derive_table_row(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let data = TableRowDeriveInput::from_derive_input(&input).expect("Wrong options");
    let stream = quote!(#data);
    stream.into()
}
