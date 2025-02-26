//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr

extern "C" {
    fn foreign();
}

unsafe extern "C" fn not_foreign() {}

fn make_extern_calls() {
    unsafe { foreign() };
    unsafe { not_foreign() };
}

#[test]
fn test() {
    make_extern_calls();
}
