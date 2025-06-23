//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn non_recursive_function_a() {}

fn non_recursive_function_b() {
    non_recursive_function_a();
}

fn non_recursive_function_c() {
    non_recursive_function_b();
}

#[test]
fn test() {
    non_recursive_function_c();
}

#[test]
fn other_test() {
    non_recursive_function_a();
}
