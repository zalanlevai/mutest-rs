//@ build
//@ stderr: empty

#![allow(unused)]

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum Base<'a> {
    Unit,
    Tuple(&'a str),
    Struct { a: u32 },
}

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
