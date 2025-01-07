//@ build
//@ stderr: empty

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

            // TEST: Reference to private constructor from the same module scope.
            impl<'a> From<()> for List {
                fn from(_: ()) -> List {
                    List([Rgb::default(); COUNT])
                }
            }

            // TEST: Reference to private constructor from a child module scope.
            mod inner {
                mod within {
                    use super::super::*;

                    fn f() {
                        let _ = List([Rgb::default(); COUNT]);
                    }
                }
            }

            // TEST: Reference to private constructor from a child module scope within another transparent item.
            mod other {
                fn f() {
                    mod within {
                        use super::super::*;
                        fn f() {
                            let _ = List([Rgb::default(); COUNT]);
                        }
                    }
                }
            }
        }
    }
}

m!();
