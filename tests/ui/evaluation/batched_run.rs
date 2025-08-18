//@ print-mutations
//@ run
//@ stdout
//@ stderr: empty
//@ mutest-flags: --mutant-batch-size=100 --mutant-batch-algorithm=greedy --mutant-batch-greedy-ordering-heuristic=none
//@ run-flags: --exhaustive

fn mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

#[test]
fn test_mutable_fn() {
    assert_eq!(5, mutable_fn(2, 3));
}

fn other_mutable_fn(a: u32, b: u32) -> u32 {
    a * b
}

#[test]
fn test_other_mutable_fn() {
    assert_eq!(6, other_mutable_fn(2, 3));
}

fn coupled_mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

fn other_coupled_mutable_fn(a: u32, b: u32) -> u32 {
    a * b
}

#[test]
fn test_coupled_mutable_fns() {
    assert_eq!(5, coupled_mutable_fn(2, 3));
    assert_eq!(6, other_coupled_mutable_fn(2, 3));
}
