//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn offset_from() {}

unsafe fn reserve_inner() {
    let _ = offset_from();
}

#[test]
fn test() {
    let _ = unsafe { reserve_inner() };
}
