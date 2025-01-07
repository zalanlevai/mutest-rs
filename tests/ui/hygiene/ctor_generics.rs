//@ build
//@ stderr: empty

#![feature(decl_macro)]

macro m() {
    struct S;
    let _ = core::marker::PhantomData::<S>;
}

#[test]
fn test() {
    m!();
}
