//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty
//@ mutest-flags: --crate-kind=mutant-for-external-tests

#[allow(unused)]
fn private_nested_fn() {}

fn private_reachable_fn() {}

#[allow(unused)]
fn private_fn() {
    private_nested_fn();
    private_reachable_fn();
}

pub fn public_and_nested_fn() {
    private_reachable_fn();
}

pub fn public_fn() {
    public_and_nested_fn();
    private_reachable_fn();
}
