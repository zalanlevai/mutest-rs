//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty
//@ rustc-flags: -C opt-level=3

fn called_from_inlined_fn() {}

#[inline]
fn inlined_fn() {
    called_from_inlined_fn();
}

#[test]
fn test() {
    inlined_fn();
}
