//@ build
//@ stderr

//! This test shows that unrepresentable non-default generic args may come from either
//! type inference from a type-relative path, or from a supertrait bound.
//!
//! Within bodies, we can place an `_` infer arg instead in such cases,
//! however that triggers type inference, which may fail in the case of ambiguity.
//!
//! Outside of bodies, we cannot use `_` infer args, and cannot resolve a valid representation.

#![allow(unused)]

mod traits {
    mod private {
        pub enum Marker {}
    }

    pub trait TraitWithPrivateMarker<T> {
        const FOO: ();
        type Foo;
        fn foo() -> Self::Foo;
    }

    impl TraitWithPrivateMarker<private::Marker> for u8 {
        const FOO: () = ();
        type Foo = u8;
        fn foo() -> Self::Foo { 1 }
    }

    pub trait Subtrait: TraitWithPrivateMarker<private::Marker> {
        fn bar();
    }
}

mod other {
    use crate::traits::TraitWithPrivateMarker;

    fn test() {
        // TEST: Inferred private generic arg of type-relative path.
        u8::foo();
        match () { u8::FOO => {} }
    }

    // TEST: Private generic arg from a supertrait bound.
    impl crate::traits::Subtrait for u8 {
        fn bar() {
            let _: Self::Foo = 1;
        }
    }
}
