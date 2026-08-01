//@ build
//@ stderr: empty
//@ aux-build: transitive_dep.rs
//@ aux-build: proxy.rs

#![feature(decl_macro)]
#![allow(unused)]

mod private {
    extern crate proxy;
}

macro m() {
    mod inner {
        extern crate proxy;

        use proxy::transitive_dep::Struct;

        const STRUCT: proxy::transitive_dep::Struct = proxy::transitive_dep::Struct;

        #[test]
        fn test() {
            let _ = proxy::transitive_dep::Struct;
        }

        mod nested {
            mod deeper {
                #[test]
                fn test() {
                    let _ = super::super::proxy::transitive_dep::Struct;
                }
            }
        }
    }

    mod other {
        mod private {
            extern crate proxy;
        }
        mod nested {
            pub(super) extern crate proxy;
        }

        #[test]
        fn test() {
            let _ = nested::proxy::transitive_dep::Struct;
        }
    }

    const _: () = {
        mod nested {
            pub extern crate proxy;
        }
        let _ = || nested::proxy::transitive_dep::Struct;
    };
}

m!();
