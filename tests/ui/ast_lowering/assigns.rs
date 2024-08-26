//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    _ = 0;
    (_, _) = (0, 1);

    let mut a = 0;
    a = 1;
    ((a), _) = (1, ());

    let mut b = 1;
    (a, b) = (2, 3);

    (_, b) = (4, 5);

    [a, b] = [0, 1];

    struct S(usize, usize);
    S(a, b) = S(1, 2);

    struct R { a: usize, b: usize }
    R { a, b } = R { a: 1, b: 2 };
}

#[test]
fn test() {
    m!();
}
