//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

struct S;

trait I {
    fn trait_f();
}

macro m() {
    impl S {
        pub(crate) fn inherent_f() {}
    }

    impl I for S {
        fn trait_f() {}
    }

    mod m {
        pub(super) fn f() {
            use crate::I;

            super::S::inherent_f();
            super::S::trait_f();

            <super::S>::inherent_f();
            <super::S>::trait_f();

            // TEST: Relative path to assoc item in inherent impl.
            #[allow(non_local_definitions)]
            impl super::S {
                fn inherent_g() {}
            }
            super::S::inherent_g();
            <super::S>::inherent_g();
        }
    }

    #[test]
    fn test() {
        m::f();
    }
}

m!();
