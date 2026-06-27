//@ print-mutations
//@ run
//@ stdout
//@ stderr: empty
//@ mutest-flags: --parallel-mutants
//@ run-env: RUST_TEST_THREADS=2
//@ run-flags: --exhaustive --use-thread-pool

fn mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

#[test]
fn test() {
    assert_eq!(5, mutable_fn(2, 3));
}
