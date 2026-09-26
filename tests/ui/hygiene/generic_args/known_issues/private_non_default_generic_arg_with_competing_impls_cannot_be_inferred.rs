//@ build: fail
//@ stderr

//! This test shows that replacing an unrepresentable non-default generic arg with an infer arg
//! does not work if competing impls result in ambiguity.
//! This can happen if the generic arg was named explicitly in a macro expansion,
//! but is not representable at the expansion site.

#![feature(decl_macro)]

#![allow(unused)]

mod traits {
    mod private {
        pub enum A {}
        pub enum B {}
    }

    pub trait Trait<T> {
        const FOO: ();
        fn foo() -> u8;
    }

    impl Trait<private::A> for u8 {
        const FOO: () = ();
        fn foo() -> u8 { 1 }
    }
    impl Trait<private::B> for u8 {
        const FOO: () = ();
        fn foo() -> u8 { 2 }
    }

    pub macro m() {
        fn test() {
            // TEST: Explicit generic arg with competing impls, which is inaccessible in expansion scope.
            <u8 as Trait<private::A>>::foo();
            match () { <u8 as Trait<private::A>>::FOO => {} }
        }
    }
}

mod expn {
    crate::traits::m!();
}
