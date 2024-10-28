//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

#[test]
fn test() {
    trait EmptyFn<T: Fn()> {}
    trait FnOnlyReturn<T: Fn() -> i8> {}
    trait FnSingleParam<T: Fn(i32)> {}
    trait FnMultiParamWithReturn<T: Fn(u64, isize) -> Result<(), String>> {}
}
