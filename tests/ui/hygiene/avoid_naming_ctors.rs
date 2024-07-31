//@ print-code
//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    struct A;
    trait I {
        fn f(&self) -> A { A }
    }
    impl I for () {
        fn f(&self) -> A { A }
    }
}

fn f() {
    m!();
}
