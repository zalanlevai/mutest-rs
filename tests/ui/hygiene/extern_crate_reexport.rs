//@ build
//@ stderr: empty
//@ aux-build: dummy_crate.rs
//@ aux-build: dummy_crate_reexport.rs
//@ rustc-flags: --extern dummy_crate_reexport
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro m() {
    dummy_crate_reexport::reexport::dummy_foo();
}

#[test]
fn test() {
    m!();
}
