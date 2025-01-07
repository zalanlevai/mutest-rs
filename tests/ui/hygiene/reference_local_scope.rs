//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    const _CONST: () = {
        struct S {
            s: Option<Box<S>>,
        }
    };

    static _STATIC: () = {
        static _INNER: () = {
            let _: &() = &_STATIC;
        };
    };
}

#[test]
fn test() {
    m!();
    const _CONST: () = ();
    static _STATIC: () = ();
}
