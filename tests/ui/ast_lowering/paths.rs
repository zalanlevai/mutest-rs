//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    struct S;
    impl S {
        fn inherent_f() {}
    }
    trait I {
        fn trait_f();
    }
    impl I for S {
        fn trait_f() {}
    }

    let _: &[Result<(), S>] = &[];
    let _: &[std::result::Result<(), S>] = &[];

    <S>::inherent_f();
    <S>::trait_f();

    <[usize]>::sort(&mut vec![3, 1, 2]);
}

#[test]
fn test() {
    m!();
}
