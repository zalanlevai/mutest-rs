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
