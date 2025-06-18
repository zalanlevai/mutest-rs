//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait TraitWithLifetimeParam<'a> {
        type Value;
        fn f() -> Self::Value;
    }

    // TEST: Early-bound lifetime parameter from impl block.
    struct StructWithLifetimeParam<'a>(std::marker::PhantomData<&'a ()>);
    impl<'a> TraitWithLifetimeParam<'a> for StructWithLifetimeParam<'a> {
        type Value = ();
        // NOTE: `Self` triggers the hygienic expansion into `StructWithLifetimeParam<'a>`.
        fn f() -> Self::Value {}
    }
}

m!();
