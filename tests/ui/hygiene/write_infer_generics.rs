//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro m() {
    let _: Vec<()> = <[_]>::into_vec(Box::new([]));
    let _: Vec<usize> = <[_]>::into_vec(Box::new([]));

    let _ = String::from("str");
    let _ = u8::from(false);
}

#[test]
fn test() {
    m!();
}
