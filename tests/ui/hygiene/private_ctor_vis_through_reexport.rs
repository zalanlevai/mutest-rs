//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    mod display {
        use color::{List, Rgb};

        mod color {
            #[derive(Clone, Copy, Default)]
            pub struct Rgb(u32, u32, u32);

            const COUNT: usize = 269;
            pub struct List([Rgb; COUNT]);

            impl<'a> From<()> for List {
                fn from(_: ()) -> List {
                    List([Rgb::default(); COUNT])
                }
            }
        }
    }
}

m!();
