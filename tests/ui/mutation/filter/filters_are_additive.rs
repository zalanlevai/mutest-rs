//@ print-targets
//@ print-mutations
//@ stdout
//@ stderr: empty
//@ mutest-flags: -v --filter-mutations=file:tests/ui/mutation/filter/filters_are_additive.rs:8,def:other_mutable_fn

fn mutable_fn(a: u32, b: u32) -> u32 {
    if a == b { return 0; }
    a + b
}

fn other_mutable_fn(a: u32, b: u32) -> u32 {
    a + b
}

#[test]
fn test() {
    assert_eq!(5, mutable_fn(2, 3));
    assert_eq!(5, other_mutable_fn(2, 3));
}
