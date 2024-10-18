//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    struct S;

    impl S {
        fn f() {
            Self::g();
        }

        fn g() {}
    }

    trait I {
        fn j();
        fn k();
    }

    impl I for S {
        fn j() {
            Self::k();
        }

        fn k() {}
    }

    impl S {
        fn h() {
            Self::f();
        }
    }
}

#[test]
fn test() {
    m!();
}
