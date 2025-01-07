//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

struct A;
struct B { a: A }

macro m() {
    mod inner {
        static SIMPLE: super::A = super::A;
    }

    static A_FROM_S: A = {
        struct S { a: A }
        let s = S { a: A };
        s.a
    };

    static A_FROM_S_THROUGH_B: A = {
        struct S { a: A, b: B }
        let s = S { a: A, b: B { a: A } };
        s.b.a
    };
}

m!();
