//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: bool_expr_negate

fn f() {
    let _: bool = Default::default();
}

#[test]
fn test() {
    f();
}
