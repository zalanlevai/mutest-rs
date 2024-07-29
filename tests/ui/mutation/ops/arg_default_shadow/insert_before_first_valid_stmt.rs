//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: arg_default_shadow

#![feature(decl_macro)]

#![allow(unused_variables)]

macro dummy() {}

macro m($a:ident) {
    println!("m: {}", $a);
}

fn f(a: usize) {
    dummy!();
    m!(a);
}

#[test]
fn test() {
    f(1);
}
