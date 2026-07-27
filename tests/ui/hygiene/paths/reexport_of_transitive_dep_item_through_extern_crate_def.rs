//@ build
//@ stderr: empty
//@ aux-build: transitive_dep.rs
//@ aux-build: proxy.rs

#![no_std]

extern crate proxy;

#[test]
fn test() {
    let _ = proxy::transitive_dep::Struct;
}
