//@ print-targets
//@ print-mutants
//@ run
//@ stdout
//@ stderr: empty
//@ run-flags: --exhaustive

fn non_mutable_fn() {}

#[test]
fn test() {
    assert_eq!((), non_mutable_fn());
}
