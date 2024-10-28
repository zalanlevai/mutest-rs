//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

#[test]
fn test() {
    for i in 0..10 { () }
    for (i, v) in ('a'..='z').enumerate() { () }
}
