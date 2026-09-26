//@ build
//@ stderr

//! This test shows that explicit macro-expanded inherent impl self ty and trait generic args
//! may be unrepresentable in the expansion scope.
//!
//! Within bodies, we can place an `_` infer arg instead in such cases,
//! however that triggers type inference, which may fail in the case of ambiguity.
//!
//! Outside of bodies, we cannot use `_` infer args, and cannot resolve a valid representation.

#![feature(decl_macro)]

#![allow(unused)]

mod def {
    mod private {
        pub struct Marker;
    }

    pub fn make_private_marker() -> private::Marker { private::Marker }

    pub struct Wrapper<T>(pub T);

    impl<T> Wrapper<T> {
        pub fn wrap(value: T) -> Self { Wrapper(value) }
    }

     pub trait Trait<T> {
        const FOO: ();
        fn foo() -> u8;
    }

    impl Trait<private::Marker> for u8 {
        const FOO: () = ();
        fn foo() -> u8 { 1 }
    }

    pub macro m() {
        fn test() {
            // TEST: Explicit self ty generic arg in type-relative path to inherent impl assoc fn,
            //       which is inaccessible in expansion scope.
            let _ = Wrapper::<private::Marker>::wrap(make_private_marker());

            // TEST: Explicit trait generic arg, which is inaccessible in expansion scope.
            <u8 as Trait<private::Marker>>::foo();
            match () { <u8 as Trait<private::Marker>>::FOO => {} }
        }
    }
}

mod expn {
    crate::def::m!();
}
