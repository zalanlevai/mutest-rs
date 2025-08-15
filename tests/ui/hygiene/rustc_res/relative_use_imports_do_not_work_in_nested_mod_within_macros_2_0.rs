//@ build: fail
//@ stderr
//@ mutest-flags: --Zno-sanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    mod def {
        pub fn f() {}

        pub mod inner {
            pub mod within {
                pub fn g() {}
            }

            pub macro m() {
                pub mod within_macro {
                    use super::super::f;
                    use super::within::*;
                }
            }
        }
    }

    def::inner::m!();
}

m!();
