//@ print-targets
//@ stdout
//@ stderr: empty
//@ aux-build: crate_with_calls.rs

extern crate crate_with_calls;

use std::iter::{self, FromIterator};

struct Collectable;

fn from_iter_impl() {}
impl FromIterator<()> for Collectable {
    fn from_iter<T>(_iter: T) -> Self {
        from_iter_impl();
        Collectable
    }
}

#[test]
fn test_local_callee_through_std() {
    let _ = iter::repeat(()).take(3).collect::<Collectable>();
}

struct ImplsExternTrait;

fn extern_trait_fn_impl() {}
impl crate_with_calls::ExternTrait for ImplsExternTrait {
    fn extern_trait_fn() {
        extern_trait_fn_impl();
    }
}

#[test]
fn test_local_callee_through_extern_crate() {
    crate_with_calls::extern_fn_calling_trait_fn::<ImplsExternTrait>();
}
