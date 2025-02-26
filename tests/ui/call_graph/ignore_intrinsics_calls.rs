//@ print-call-graph
//@ stderr: empty

#![allow(internal_features)]
#![feature(core_intrinsics)]

fn make_intrinsics_call() {
    let _ = core::intrinsics::caller_location();
}

#[test]
fn test() {
    make_intrinsics_call();
}
