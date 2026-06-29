//@ print-targets
//@ print-mutations
//@ stdout
//@ stderr: empty
//@ mutest-flags: -v --filter-mutations=file:tests/ui/mutation/filter/mutate_exprs_partially_in_file_line_ranges.rs:12:13

fn other_mutable_fn(a: bool, b: bool) -> bool {
    a || b
}

fn mutable_fn(a: u32, b: u32) -> bool {
    other_mutable_fn(
        a == 0,
        b >= 1,
    )
}

#[test]
fn test() {
    assert!(mutable_fn(0, 1));
}
