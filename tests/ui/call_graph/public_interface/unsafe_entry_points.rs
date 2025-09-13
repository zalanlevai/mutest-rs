//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty
//@ mutest-flags: --crate-kind=mutant-for-external-tests

fn private_nested_tainted_fn() {}

fn private_tainted_fn() {
    private_nested_tainted_fn();
}

pub fn public_nested_tainted_fn() {}

pub fn public_tainted_fn() {
    public_nested_tainted_fn();
}

pub unsafe fn unsafe_entry() {
    private_tainted_fn();
    public_tainted_fn();
}
