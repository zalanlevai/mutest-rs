//@ build
//@ stderr: empty
//@ aux-build: transitive_dep.rs
//@ aux-build: proxy.rs

#![no_std]
#![allow(unused)]

extern crate proxy;

use proxy::transitive_dep::Struct;
