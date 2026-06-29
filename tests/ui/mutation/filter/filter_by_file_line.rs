//@ print-targets
//@ print-mutations
//@ stdout
//@ stderr: empty
//@ mutest-flags: -v --filter-mutations=file:tests/ui/mutation/filter/filter_by_file_line.rs:9

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
