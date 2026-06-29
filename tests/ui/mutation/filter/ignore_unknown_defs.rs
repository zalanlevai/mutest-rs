//@ print-targets
//@ print-mutations
//@ stdout
//@ stderr: empty
//@ mutest-flags: -v --filter-mutations=def:other_mutable_fn

fn mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

#[test]
fn test() {
    assert_eq!(5, mutable_fn(2, 3));
}
