//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

fn impl_drop() {}

fn pseudo_drop() {
    impl_drop();
}

#[test]
fn test_direct() {
    pseudo_drop();
}

fn indirect_drop() {
    pseudo_drop();
}

#[test]
fn test_indirect() {
    indirect_drop();
}
