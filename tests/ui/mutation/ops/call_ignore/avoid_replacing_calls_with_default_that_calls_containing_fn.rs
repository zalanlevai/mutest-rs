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
        S::new_with_args(0, false)
    }
}

impl S {
    fn new(_: ()) -> Self {
        Self { v: 0, b: false }
    }

    fn new_with_args(v: usize, b: bool) -> Self {
        let mut s = S::new(());
        s.v = v;
        s.b = b;
        s
    }
}

fn f() {
    let default_s = S::new_with_args(0, false);
}

#[test]
fn test() {
    f();
}
