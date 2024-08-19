//@ build
//@ mutest-flags: --Zsanitize-macro-expns

#![allow(unused)]

#[derive(Default)]
enum E {
    #[default]
    A,
    B,
    C,
}

#[derive(Debug)]
struct DebugThreeFields {
    a: u32,
    b: f64,
    c: isize,
}
