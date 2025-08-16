//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_delete, call_value_default_shadow

#![allow(unused)]

struct S {
    v: usize,
    b: bool,
}

impl Default for S {
    fn default() -> Self {
        Self { v: 0, b: false }
    }
}

impl S {
    fn new() -> Self {
        Self { v: 0, b: false }
    }

    fn new_with_args(v: usize, b: bool) -> Self {
        Self { v, b }
    }

    fn v(&self) -> usize {
        self.v
    }
}

fn mk_default_s() -> S {
    S { v: 0, b: false }
}

fn mk_s_with_args(v: usize, b: bool) -> S {
    S { v, b }
}

fn f() {
    let default_s = S::new();
    let non_default_s = S::new_with_args(0, false);
    let non_default_usize = default_s.v();
    let default_s = mk_default_s();
    let non_default_s = mk_s_with_args(0, false);
}

#[test]
fn test() {
    f();
}
