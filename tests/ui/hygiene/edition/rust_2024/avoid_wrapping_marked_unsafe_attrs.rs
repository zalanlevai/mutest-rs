//@ build
//@ stderr: empty
//@ edition: 2024
//@ aux-build: unsafe_attr_macros.rs

extern crate unsafe_attr_macros;

unsafe_attr_macros::marked!();
