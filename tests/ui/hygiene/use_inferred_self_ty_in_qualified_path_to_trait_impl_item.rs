//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

//! This test shows that even for non-qualified associated item paths to trait impl items,
//! it is sometimes necessary to use the inferred self type from the type system.
//!
//! This test is based on alacritty's `RenderableCursor::new_hidden` function,
//! and is triggered by the omission of the inferrable type parameter to
//! the `Point` type.

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    #[derive(Default)]
    struct Point<L = ()> {
        line: L,
    }

    struct RenderableCursor {
        point: Point<usize>,
    }

    impl RenderableCursor {
        fn new_hidden() -> Self {
            let point = Point::default();
            Self { point }
        }
    }
}

#[test]
fn test() {
    m!();
}
