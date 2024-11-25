//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    struct Group;
    impl Group {
        const WIDTH: usize = 8;

        const fn empty() -> [u8; Group::WIDTH] {
            [0; Group::WIDTH]
        }
    }
}

m!();
