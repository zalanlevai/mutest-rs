//@ build
//@ stderr: empty

#![feature(decl_macro)]

use std::convert::TryFrom;

macro m() {
    let _ = <[Vec<u32>; 12]>::try_from(vec![vec![]; 12]).unwrap();
}

#[test]
fn test() {
    m!();
}
