//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

trait A {
    fn f() {}
}

macro m() {
    trait B {
        type A: crate::A;
    }

    struct S;
    impl A for S {}
    impl B for S { type A = S; }

    fn f() {
        <<S as B>::A as A>::f();
        <S as B>::A::f();
    }

    impl S {
        fn g() {
            <Self as B>::A::f();
        }
    }
}

m!();
