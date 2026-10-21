//@ build
//@ stderr: empty

#![feature(decl_macro)]
#![allow(unused)]

// NOTE: Active `cfg` and `cfg_attr` attributes leave synthetic trace attributes on the item.
#[cfg(all())]
use std::fmt::Write;

#[cfg_attr(all(), allow(unused_imports))]
use std::fmt::Display;

macro m() {
    #[cfg(all())]
    use std::fmt::Debug;

    #[cfg_attr(all(), allow(unused_imports))]
    use std::hash::Hash;
}

m!();

fn render(n: usize) -> String {
    let mut out = String::new();
    write!(out, "{}", n + 1).unwrap();
    out
}

#[test]
fn test() {
    assert_eq!(render(1), "2");
}
