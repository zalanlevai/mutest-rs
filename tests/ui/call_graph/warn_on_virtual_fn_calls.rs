//@ print-targets
//@ stdout
//@ stderr

fn impl_call() {}

fn make_virtual_fn_call(f: &dyn Fn()) {
    f();
}

fn make_virtual_fn_mut_call(f: &mut dyn FnMut(usize) -> usize) {
    let _ = f(1);
}

#[test]
fn test() {
    make_virtual_fn_call(&|| impl_call());
    make_virtual_fn_mut_call(&mut |v| v + 1);

    let f: &dyn Fn() = &|| impl_call();
    f();
}
