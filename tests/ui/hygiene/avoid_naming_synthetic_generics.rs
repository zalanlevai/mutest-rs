//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait Trait {}

    struct S;
    impl<T: Trait> From<T> for S {
        fn from(_v: T) -> Self { Self }
    }

    fn f(v: impl Trait) {
        let _ = S::from(v);
    }
}

m!();
