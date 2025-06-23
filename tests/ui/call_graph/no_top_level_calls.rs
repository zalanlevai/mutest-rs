//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

#[test]
fn empty_test() {}

#[test]
fn test_with_basic_stmts() {
    let (_x, _y, _z) = (1, 2, 3);
}
