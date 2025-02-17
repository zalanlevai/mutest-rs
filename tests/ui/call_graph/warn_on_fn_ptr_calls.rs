//@ print-targets
//@ stdout
//@ stderr

fn impl_call() {}

fn make_fn_ptr_call(f: fn()) {
    f();
}

#[test]
fn test() {
    make_fn_ptr_call(impl_call);
}
