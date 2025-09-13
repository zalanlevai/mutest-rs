//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr
//@ mutest-flags: --crate-kind=mutant-for-external-tests

pub trait Trait {
    fn assoc_fn();
}

pub fn public_generic_fn<T: Trait>() {
    T::assoc_fn()
}
