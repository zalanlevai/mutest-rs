//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn recursive_function(i: usize) {
    if i == 1 { return; }
    recursive_function(i - 1);
}

fn other_recursive_function(i: usize) {
    recursive_function(i);
}

#[test]
fn test() {
    recursive_function(3);
    other_recursive_function(3);
}
