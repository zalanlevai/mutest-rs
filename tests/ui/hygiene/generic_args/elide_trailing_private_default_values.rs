//@ build
//@ stderr: empty

#![allow(unused)]

mod traits {
    mod private {
        pub enum Marker {}
    }

    pub trait MarkerOnlyTrait<M = private::Marker> {
        type Type;
        fn foo() -> Self::Type;
    }

    pub trait Trait<T, M = private::Marker> {
        type Type;
        fn foo() -> Self::Type;
    }

    pub trait TraitWithMultipleDefaults<T, const N: usize, M1 = (), M2 = private::Marker, const X: usize = 0> {
        type Type;
        fn foo() -> Self::Type;
    }
}

mod other {
    impl crate::traits::MarkerOnlyTrait for i32 {
        type Type = ();
        fn foo() -> Self::Type {}
    }

    impl<T> crate::traits::Trait<T> for i32 {
        type Type = ();
        fn foo() -> Self::Type {}
    }

    impl crate::traits::TraitWithMultipleDefaults<u32, 1> for () {
        type Type = ();
        fn foo() -> Self::Type {}
    }
    impl crate::traits::TraitWithMultipleDefaults<u32, 1, bool> for () {
        type Type = ();
        fn foo() -> Self::Type {}
    }
    impl crate::traits::TraitWithMultipleDefaults<u32, 1, (), bool> for () {
        type Type = ();
        fn foo() -> Self::Type {}
    }
    impl crate::traits::TraitWithMultipleDefaults<u32, 1, (), bool, 1> for () {
        type Type = ();
        fn foo() -> Self::Type {}
    }
}
