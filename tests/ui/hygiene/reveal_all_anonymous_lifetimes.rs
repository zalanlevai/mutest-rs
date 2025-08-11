//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

use std::marker::PhantomData;

macro m() {
    trait Trait {
        type AssocTy;
        fn assoc_fn() -> Self::AssocTy;
    }

    // TEST: Name all anonymous lifetimes introduced by references in impl item.
    impl Trait for &&&&&&() {
        type AssocTy = ();
        fn assoc_fn() -> Self::AssocTy {}
    }

    // TEST: Name all anonymous lifetimes introduced by impl item.
    struct LifetimeGenericStruct<'a, 'b, 'c, 'd, T>(PhantomData<(&'a (), &'b (), &'c (), &'d(), T)>);
    impl<'named, T> Trait for &&LifetimeGenericStruct<'_, '_, '_, 'named, T> {
        type AssocTy = ();
        fn assoc_fn() -> Self::AssocTy {}
    }

    // TEST: Name anonyous lifetimes introduced by lifetime-generic trait in impl.
    trait GenericTrait<'a, 'b, 'c> {
        type AssocTy;
        fn assoc_fn() -> Self::AssocTy;
    }
    impl<'b> GenericTrait<'_, 'b, '_> for () {
        type AssocTy = ();
        fn assoc_fn() -> Self::AssocTy {}
    }

    // TEST: Ignore generic parameters that are introduced in binders within trait bounds.
    struct GenericStructWithTraitBoundLifetimeBinder<T: for<'a> Fn(&'a dyn Trait<AssocTy = ()>)>(T);
}

m!();
