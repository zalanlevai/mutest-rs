//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    fn f() {
        match [1, 2, 3, 4, 5] {
            [one, two, three, four, five] => {}
            [one, two, three, four, five, ..] => {}
            [one, two, ..] => {}
            [one, two, .., five] => {}
            [one, two, .., four, five] => {}
            [..] => {}
            [.., five] => {}
            [.., four, five] => {}
        }
    }
}

m!();
