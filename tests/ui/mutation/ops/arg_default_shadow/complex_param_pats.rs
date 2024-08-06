//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: arg_default_shadow

#![feature(decl_macro)]

#![allow(unused_variables)]

struct S<'a, T, D: Default + Copy> {
    v: Result<&'a str, usize>,
    t: T,
    d: D,
}

fn f<T, D: Default + Copy>((n, test): (usize, bool), s @ &S { v, ref t, d }: &S<'_, T, D>) {
    let _: usize = n;
    let _: bool = test;
    let _: Result<&str, usize> = v;
    let _: &T = t;
    let _: D = d;
    let _: &S<'_, T, D> = s;
}

#[test]
fn test() {
    f((1, true), &S { v: Ok("foo"), t: -1_i32, d: -1_i32 });
}
