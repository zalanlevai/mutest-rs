//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait I {
        type Internal;
    }

    struct S(<S as I>::Internal);
    impl I for S {
        type Internal = i32;
    }

    enum R {
        A(<<R as I>::Internal as I>::Internal),
    }
    impl I for R {
        type Internal = S;
    }

    type InternalAlias = <S as I>::Internal;

    struct Q;
    impl I for Q {
        type Internal = <S as I>::Internal;
    }

    trait TraitWithAssocTyBinding
    where
        <R as I>::Internal: I,
    {}

    impl TraitWithAssocTyBinding for <S as I>::Internal
    where
        <R as I>::Internal: I,
    {}

    trait GenericTrait<T: I> {}
    impl GenericTrait<<R as I>::Internal> for () {}

    fn f()
    where
        <R as I>::Internal: I,
    {
        let _: <S as I>::Internal = 1;
        let _: <<R as I>::Internal as I>::Internal = 2;
    }

    trait F {
        fn f<T: GenericTrait<<R as I>::Internal>>();
    }
}

#[test]
fn test() {
    m!();
}
