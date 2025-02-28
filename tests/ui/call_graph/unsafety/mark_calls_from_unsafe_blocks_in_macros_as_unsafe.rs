//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused_unsafe)]

fn fn_called_from_macro() {}
macro call_fn() {
    let _ = fn_called_from_macro();
}

fn fn_called_from_unsafe_block_in_macro() {}
macro call_fn_in_unsafe_block() {
    unsafe {
        let _ = fn_called_from_unsafe_block_in_macro();
    }
}

fn f() {
    unsafe {
        call_fn!();
    }

    call_fn_in_unsafe_block!();
}

#[test]
fn test() {
    let _ = f();
}
