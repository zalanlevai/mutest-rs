//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn recursive_function(i: usize) {
    if i == 1 { return; }
    make_indirect_recursive_call(i - 1);
}

fn non_recursive_function_a() {}
fn non_recursive_function_b() {}

fn non_recursive_function_c() {
    non_recursive_function_a();
    non_recursive_function_b();
}

fn make_indirect_recursive_call(i: usize) {
    recursive_function(i);

    non_recursive_function_a();
    non_recursive_function_b();
    non_recursive_function_c();
}

#[test]
fn test() {
    recursive_function(3);
}
