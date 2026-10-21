//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_value_default_shadow

#![allow(unused)]

use std::sync::Arc;

type Report = Arc<dyn Fn(&u32) + Send + Sync>;

// NOTE: The object lifetime bound of the `dyn` type in the signatures defaults to `'static`.
fn handler(_: ()) -> Option<Arc<dyn Fn(&str, &Report) -> usize + Send + Sync>> {
    None
}

fn handler_ptr(_: ()) -> Option<fn(&str, Box<dyn Fn(&u32)>) -> usize> {
    None
}

fn f() {
    let _ = handler(());
    let _ = handler_ptr(());
}

#[test]
fn test() {
    f();
}
