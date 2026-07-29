//@ build
//@ stderr: empty
//@ aux-build: transitive_dep.rs
//@ aux-build: crate_proxy.rs

#![no_std]
#![allow(unused)]

extern crate crate_proxy;

use crate_proxy::transitive_dep::Struct;
