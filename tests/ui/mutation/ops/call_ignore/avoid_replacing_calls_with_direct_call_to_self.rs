//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_delete, call_value_default_shadow

#![allow(unused)]

struct S(usize);

impl S {
    fn new(v: usize) -> Self {
        Self(v)
    }
}

impl Default for S {
    fn default() -> Self {
        // NOTE: Test with both functions and constructors, in case we ever
        //       decide to stop mutating constructor calls.
        let _ = Self::new(1);
        Self(1)
    }
}

fn f() {
    let _ = S::default();
}

#[test]
fn test() {
    f();
}
