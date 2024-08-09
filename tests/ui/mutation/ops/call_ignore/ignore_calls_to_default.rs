//@ print-mutants
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

struct G<T>(T);

impl Default for G<usize> {
    fn default() -> Self {
        Self(0)
    }
}

fn f() {
    let default_u32: u32 = Default::default();

    let default_s = S::default();

    fn default_g<T>(_: ()) -> G<T>
    where
        G<T>: Default,
    {
        G::<T>::default()
    }

    let default_g_usize = default_g::<usize>(());
}

#[test]
fn test() {
    f();
}
