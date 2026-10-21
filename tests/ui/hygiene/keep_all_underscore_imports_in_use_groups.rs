//@ build
//@ stderr: empty

#![feature(decl_macro)]
#![allow(unused)]

use std::io::{Read as _, Write as _};

fn copy_twice(mut input: &[u8]) -> Vec<u8> {
    let mut buf = Vec::new();
    input.read_to_end(&mut buf).unwrap();
    let mut out = Vec::new();
    out.write_all(&buf).unwrap();
    out.write_all(&buf).unwrap();
    out
}

macro m() {
    use std::fmt::{Debug as _, Write as _};

    fn render(n: usize) -> String {
        let mut out = String::new();
        write!(out, "{}", n).unwrap();
        out
    }
}

m!();

#[test]
fn test() {
    assert_eq!(copy_twice(b"ab"), b"abab");
}
