//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn f() {
    let _ = f_generic(1_u8);
    let _ = f_generic(0_u8);
}

fn f_generic<T>(_: T) {}

#[test]
fn test() {
    let _ = f();
    let _ = f();

    let _ = f_generic(-1_i32);
    let _ = f_generic(1_usize);
    let _ = f_generic(0_i32);
}
