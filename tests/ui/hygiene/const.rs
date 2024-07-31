//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

struct A;
struct B { a: A }

macro m() {
    mod inner {
        const SIMPLE: super::A = super::A;
    }

    const A_FROM_S: A = {
        struct S { a: A }
        let s = S { a: A };
        s.a
    };

    const A_FROM_S_THROUGH_B: A = {
        struct S { a: A, b: B }
        let s = S { a: A, b: B { a: A } };
        s.b.a
    };
}

m!();
