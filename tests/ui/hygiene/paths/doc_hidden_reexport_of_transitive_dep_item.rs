//@ build
//@ stderr: empty
//@ aux-build: transitive_dep.rs
//@ aux-build: doc_hidden_proxy.rs
//@ rustc-flags: --extern doc_hidden_proxy

//! Regression test for resolving nested paths through `#[doc(hidden)]` re-exports.
//!
//! The rustc `visible_parent_map` implementation has (at the time of writing) a bug
//! which excludes modules nested within `#[doc(hidden)]` modules from the search.
//! For more details, see our fix at https://github.com/rust-lang/rust/pull/159881
//! and the linked issue at https://github.com/rust-lang/rust/issues/159880.
//!
//! We use the `visible_parent_map` to resolve paths to items in transitive dependencies.
//! Thus, to cover even niche re-export scenarios (often from macro-generated code),
//! we must capture all `#[doc(hidden)]` re-export fallbacks.

#![no_std]

#[test]
fn test() {
    let _ = doc_hidden_proxy::__private::inner::Struct;
}
