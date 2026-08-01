//@ build
//@ stderr: empty
//@ aux-build: transitive_dep.rs
//@ aux-build: proxy.rs

#![no_std]

extern crate proxy as renamed_proxy;

mod inner {
    #[test]
    fn test() {
        let _ = renamed_proxy::transitive_dep::Struct;
    }
}
