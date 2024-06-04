#![crate_type = "proc-macro"]

extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro_derive(DeriveMacroWithAttr, attributes(foo, bar))]
pub fn derive_macro_with_attr(_input: TokenStream) -> TokenStream {
    "".parse().unwrap()
}
