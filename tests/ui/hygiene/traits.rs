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
        type Item;

        fn f() -> Self::Item;
        fn g() -> <Self as Bar>::Item;
    }

    impl Bar for A {
        type Item = ();

        fn f() -> Self::Item { () as Self::Item }
        fn g() -> <Self as Bar>::Item { () as <Self as Bar>::Item }
    }

    trait Baz {
        const LEN: usize;

        fn f() -> usize { Self::LEN }
        fn g() -> usize { <Self as Baz>::LEN }
    }

    impl Baz for A {
        const LEN: usize = 1;

        fn f() -> usize { Self::LEN }
        fn g() -> usize { <Self as Baz>::LEN }
    }
}

assoc_hygiene!();
