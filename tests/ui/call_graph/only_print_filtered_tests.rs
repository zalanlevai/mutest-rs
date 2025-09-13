//@ print-call-graph
//@ stdout
//@ stderr: empty
//@ mutest-flags: --call-graph-filter-entry-points=test1

fn nested_common() {}

fn common() {
    nested_common();
}

fn indirectly_common() {}

fn nested_only_test_1() {
    indirectly_common();
}

fn only_test_1() {
    nested_only_test_1();
}

fn nested_only_test_2() {}

fn only_test_2() {
    nested_only_test_2();
    indirectly_common();
}

#[test]
fn test1() {
    common();
    only_test_1();
}

#[test]
fn test2() {
    common();
    only_test_2();
}
