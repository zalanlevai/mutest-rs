//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro m() {
    struct S;

    let _: &[Result<(), S>] = &[];
    let _: &[std::result::Result<(), S>] = &[];
}

#[test]
fn test() {
    m!();
}
