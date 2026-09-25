//@ build
//@ stderr

//! This test shows that unrepresentable default generic args cannot be elided
//! if followed by a non-default generic arg.
//!
//! Within bodies, we can place an `_` infer arg instead in such cases,
//! which allows us to name the latter non-default generic arg.
//! However, note that while an elided default value always resolves to the default value,
//! an infer arg triggers type inference, which may fail in the case of ambiguity.
//! Because of this, we only use infer args for default generic args if
//! a following non-default generic arg forces it.
//!
//! Outside of bodies, we cannot use `_` infer args, and cannot resolve a valid representation.

#![allow(unused)]

mod traits {
    mod private {
        pub enum Marker {}
    }

    pub trait TraitWithPrivateMarkerAndConst<T, M = private::Marker, const N: usize = 0> {
        const FOO: ();
        fn foo();
    }

    impl TraitWithPrivateMarkerAndConst<u32, private::Marker, 1> for u8 {
        const FOO: () = ();
        fn foo() {}
    }

    pub trait TraitWithPrivateMarkerAndTy<M = private::Marker, T = ()> {
        const BAR: ();
        fn bar();
    }

    impl TraitWithPrivateMarkerAndTy<private::Marker, u8> for u8 {
        const BAR: () = ();
        fn bar() {}
    }
}

mod expn {
    use crate::traits::{TraitWithPrivateMarkerAndConst, TraitWithPrivateMarkerAndTy};

    fn test() {
        // TEST: Inferred private default value, followed by non-default const value.
        u8::foo();
        match () { u8::FOO => {} }

        // TEST: Inferred private default value, followed by non-default ty value.
        u8::bar();
        match () { u8::BAR => {} }
    }
}
