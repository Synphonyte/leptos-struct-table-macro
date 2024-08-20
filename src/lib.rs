mod models;
mod table_row;

use darling::FromDeriveInput;
use models::TableRowDeriveInput;
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse_macro_input;

#[proc_macro_derive(TableRow, attributes(table))]
pub fn derive_table_row(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    let data = TableRowDeriveInput::from_derive_input(&input);
    let stream = match data {
        Ok(data) => data.into_token_stream(),
        Err(err) => err.write_errors(),
    };
    stream.into()
}
