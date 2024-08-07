//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: arg_default_shadow

fn f<D: Default>(_: usize, _: bool, (_, (_, _)): (Result<&str, usize>, (D, ()))) {
    print!("");
}

#[test]
fn test() {
    f(1, true, (Ok("foo"), (-1_i32, ())));
}
