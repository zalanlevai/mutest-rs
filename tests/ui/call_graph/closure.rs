//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn impl_closure() {}

fn make_closure_call<F: Fn()>(f: F) {
    f();
}

#[test]
fn test() {
    let f = || impl_closure();
    f();

    make_closure_call(|| impl_closure());
}
