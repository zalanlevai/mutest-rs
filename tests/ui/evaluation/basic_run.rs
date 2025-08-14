//@ print-mutants
//@ run
//@ stdout
//@ stderr: empty
//@ run-flags: --exhaustive

fn mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

#[test]
fn test() {
    assert_eq!(5, mutable_fn(2, 3));
}
