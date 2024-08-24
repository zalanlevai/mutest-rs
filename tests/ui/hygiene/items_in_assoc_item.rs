//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

macro m() {
    #[allow(unused)]
    trait I {
        fn f();
    }

    impl I for () {
        fn f() {
            const _CONST: usize = 1;
            let _: usize = _CONST;

            static _STATIC: u32 = 1;
            let _: u32 = _STATIC;

            struct S;
            let _: S = S;
        }
    }
}

m!();
