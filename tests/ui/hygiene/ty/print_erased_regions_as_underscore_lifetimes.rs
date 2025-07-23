//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait NestedTrait {
        fn nested_assoc_fn() -> Self;
    }

    trait Trait {
        type GenericAssocTy<'a>: NestedTrait;
    }

    // TEST: In the path referencing the assoc ty `Trait::GenericAssocTy`,
    //       the region parameter is an erased region.
    //       To create a valid path, this erased region must be represented
    //       as the `'_` lifetime.
    fn test_call_to_nested_assoc_fn_through_generic_assoc_ty<T>()
    where
        T: Trait
    {
        let _ = T::GenericAssocTy::nested_assoc_fn();
    }
}

m!();
