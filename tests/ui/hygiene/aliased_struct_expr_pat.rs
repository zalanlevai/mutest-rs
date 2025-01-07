//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    struct S<T> {
        v: T,
    }

    type AliasedS<T> = S<T>;

    let s = AliasedS { v: -1 };
    match s {
        AliasedS { v: _ } => {}
    }
}

#[test]
fn test() {
    m!();
    struct S;
    type AliasedS = S;
}
