//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_delete

#![allow(unused)]

struct S<T> {
    v: usize,
    t: T,
}

impl<T: Default> Default for S<T> {
    fn default() -> Self {
        Self { v: 0, t: Default::default() }
    }
}

fn f() {
    fn g(_: ()) -> S<usize> {
        S { v: 0, t: 1 }
    }

    let _ = g(());

    fn h<T>(t: T) -> S<T> {
        S { v: 0, t }
    }

    let _ = h(1_i32);
    let _ = h(&mut false);
}

#[test]
fn test() {
    f();
}
