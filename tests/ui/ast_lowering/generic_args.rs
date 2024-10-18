//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait EmptyFn<T: Fn()> {}
    trait FnOnlyReturn<T: Fn() -> i8> {}
    trait FnSingleParam<T: Fn(i32)> {}
    trait FnMultiParamWithReturn<T: Fn(u64, isize) -> Result<(), String>> {}
}

#[test]
fn test() {
    m!();
}
