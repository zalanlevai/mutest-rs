//@ edition: 2024

#![crate_type = "proc-macro"]

#![feature(proc_macro_quote)]

extern crate proc_macro;

use proc_macro::{Ident, Span, TokenStream, TokenTree, quote};

#[proc_macro_attribute]
pub fn quickcheck(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut original_ident = None;

    let prop_fn = input.into_iter()
        .map(|token_tree| {
            // HACK: Find first ident, ignoring the `fn` keyword.
            if let TokenTree::Ident(ident) = &token_tree && ident.to_string() != "fn" && let None = &original_ident {
                original_ident = Some(ident.clone());
            }
            token_tree
        })
        .collect::<TokenStream>();

    let Some(ident) = original_ident else { panic!("cannot find prop fn ident"); };

    quote! {
        #[test]
        fn $ident() {
            $prop_fn
            ::quickcheck::quickcheck($ident as fn(u32, u32) -> bool);
        }
    }
}
