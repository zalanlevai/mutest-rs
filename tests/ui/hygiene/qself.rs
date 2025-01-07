//@ build
//@ stderr: empty

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

    enum E {
        A,
        B,
    }
    impl A for E {}
    impl B for E { type A = E; }

    fn f() {
        <<S as B>::A as A>::f();
        <S as B>::A::f();

        match E::A {
            <E as B>::A::A => {}
            <E as B>::A::B => {}
        }
    }

    impl S {
        fn g() {
            <Self as B>::A::f();
        }
    }

    fn h() {
        struct GenericS<'a, T>(std::marker::PhantomData<&'a T>);
        impl<'a, T> B for GenericS<'a, T> { type A = S; }
        let _: <GenericS<'static, S> as B>::A = S;
    }
}

m!();
