//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro m() {
    // <[$ty]>::into_vec::<_>(Box::new([]));
    let _: Vec<()> = <[_]>::into_vec(Box::new([]));
    let _: Vec<usize> = <[_]>::into_vec(Box::new([]));

    // <$ty as From<_>>::from($expr);
    let _ = String::from("str");
    let _ = u8::from(false);
}

#[test]
fn test() {
    m!();
}
