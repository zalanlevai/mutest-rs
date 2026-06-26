//@ print-tests
//@ print-targets
//@ stdout
//@ stderr: empty
//@ aux-build: quickcheck.rs
//@ aux-build: quickcheck_macros.rs

extern crate quickcheck;
extern crate quickcheck_macros;

use quickcheck_macros::quickcheck;

fn mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

quickcheck::quickcheck! {
    fn quickcheck_decl_macro_test(a: u32, b: u32) -> bool {
        a + b == mutable_fn(a, b)
    }
}

#[quickcheck]
fn quickcheck_attr_macro_test(a: u32, b: u32) -> bool {
    a + b == mutable_fn(a, b)
}
