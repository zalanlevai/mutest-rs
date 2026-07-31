//@ build
//@ stderr: empty

//! The `alloc` standard library crate is not implicitly injected into the extern prelude,
//! unlike `core` and `std` (unless `#![no_std]`), and using it requires `extern crate alloc;`.
//! However, certain macros in `std` expand to references that are defined in `alloc` and
//! not re-exported elsewhere, such as `vec!`'s expansion using `alloc::intrinsics::write_box_via_move`.
//! Thus, we need to inject the `alloc` crate into the extern prelude.
//!
//! This test ensures that the injected `alloc` crate does not conflict with any of the crate's own declarations,
//! which could be possible if the `alloc` crate is not explicitly pulled into the extern prelude.

mod alloc {
    #![allow(unused)]

    pub(crate) mod vec {
        pub(crate) struct Vec;
    }

    pub(crate) mod string {
        pub(crate) enum String {}
    }
}

#[test]
fn test_vec() {
    let _ = Vec::<()>::new();
}

#[test]
fn test_string() {
    fn alloc_string() -> String {
        "string".to_owned()
    }
    let _ = alloc_string();
}
