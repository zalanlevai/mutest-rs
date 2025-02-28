//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

#![allow(unused_unsafe)]

fn offset_from() {}

fn reserve_inner() {
    unsafe {
        let _ = offset_from();
    }
}

#[test]
fn test() {
    let _ = reserve_inner();
}
