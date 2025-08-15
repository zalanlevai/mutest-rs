//@ print-mutants
//@ run: fail
//@ stdout
//@ stderr: empty
//@ run-flags: --exhaustive

fn mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

#[test]
fn non_asserting_test() {
    mutable_fn(2, 3);
}
