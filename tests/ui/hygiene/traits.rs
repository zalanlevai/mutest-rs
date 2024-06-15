//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

trait Foo {
    type Item;

    fn f() -> Self::Item;
}

macro assoc_hygiene() {
    struct A;

    impl Foo for A {
        type Item = A;

        fn f() -> Self::Item { A }
    }

    trait Bar {
        const LEN: usize;

        fn f() -> usize { Self::LEN }
    }
}

assoc_hygiene!();
