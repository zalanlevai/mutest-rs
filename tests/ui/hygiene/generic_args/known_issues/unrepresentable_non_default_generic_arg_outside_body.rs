//@ build: fail
//@ stderr

//! This test shows that unrepresentable non-default generic args outside of bodies, e.g., item signatures,
//! cause a fatal expansion error, as we cannot replace them with the necessary `_` infer args,
//! since they are disallowed in those positions.

#![feature(decl_macro)]

#![allow(unused)]

mod traits {
    mod private {
        pub enum Marker {}
    }

    pub trait Trait<T> {
        type Assoc;
    }

    impl Trait<private::Marker> for () {
        type Assoc = ();
    }

    pub trait Subtrait: Trait<private::Marker> {
        fn assoc() -> Self::Assoc;
    }

    pub macro m() {
        // TEST: Unrepresentable generic arg from a supertrait bound in a trait impl, in item sig position.
        impl Subtrait for () {
            fn assoc() -> Self::Assoc {}
        }
    }
}

mod expn {
    crate::traits::m!();
}
