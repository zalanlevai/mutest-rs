//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn recursive_function(i: usize) {
    if i == 1 { return; }
    recursive_function(i - 1);
}

#[test]
fn test_direct() {
    recursive_function(3);
}

fn indirect_recursive() {
    recursive_function(2);
}

#[test]
fn test_indirect() {
    indirect_recursive();
}
