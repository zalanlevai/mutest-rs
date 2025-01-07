//@ build
//@ stderr: empty

#![feature(decl_macro)]
#![feature(f16)]
#![feature(f128)]

#![allow(unused)]

macro m() {
    // NOTE: The `rustc_resolve` path segment resolution of primitive types changes
    //       in the presence of other definitions in scope with the same name
    //       (e.g. imports, modules, type aliases, struct definitions, etc.)
    //       from `Res::PrimTy` to `Res::Def`.
    //       However, the resolution of the entire node is still correctly `Res::PrimTy`,
    //       so we have to prefer using this information when available.
    use std::{char, u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize, f16, f32, f64, f128, str};

    const CHAR: char = ' ';
    const U8: u8 = 0;
    const U16: u16 = 0;
    const U32: u32 = 0;
    const U64: u64 = 0;
    const U128: u128 = 0;
    const USIZE: usize = 0;
    const I8: i8 = 0;
    const I16: i16 = 0;
    const I32: i32 = 0;
    const I64: i64 = 0;
    const I128: i128 = 0;
    const ISIZE: isize = 0;
    const F16: f16 = 0.0;
    const F32: f32 = 0.0;
    const F64: f64 = 0.0;
    const F128: f128 = 0.0;
    const STR: &str = "";
}

#[test]
fn test() {
    m!();
}
