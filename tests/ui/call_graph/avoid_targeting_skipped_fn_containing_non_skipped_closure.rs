//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn impls_g() {}

#[mutest::skip]
fn f() {
    let g = || impls_g();
    g();
}

#[test]
fn test() {
    f();
}
