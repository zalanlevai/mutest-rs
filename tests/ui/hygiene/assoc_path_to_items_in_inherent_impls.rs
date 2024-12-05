//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

use std::marker::PhantomData;

macro m() {
    // TEST: Reference to assoc item in inherent impl for simple struct type.
    struct NoGenerics;
    impl NoGenerics {
        fn f() {}
    }
    NoGenerics::f();

    // TEST: Reference to assoc item in inherent impl for generic struct type.
    struct OneTyGeneric<T>(PhantomData<T>);
    impl<T> OneTyGeneric<T> {
        fn f() {}
    }
    OneTyGeneric::<()>::f();

    // TEST: Reference to assoc item in inherent impl for dyn trait type.
    trait OneTyGenericTrait<T> {}
    impl<T> dyn OneTyGenericTrait<T> {
        fn f() {}
    }
    <dyn OneTyGenericTrait<()>>::f();

    // TEST: Reference to assoc item in inherent impl for slice type.
    let mut xs = [3, 1, 2];
    <[_]>::sort(&mut xs);
}

#[test]
fn test() {
    m!();
}
