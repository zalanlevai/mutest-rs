//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

#[test]
fn test() {
    let _ = || {};
    let _ = || -> i32 { -1 };
    let _ = |a: usize| { () };
    let _ = |a: &str| -> usize { a.len() };
    let _ = |(a, b): (String, i8)| {};
}
