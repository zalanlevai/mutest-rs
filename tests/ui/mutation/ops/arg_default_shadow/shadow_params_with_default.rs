//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: arg_default_shadow

struct S;

fn f<T, D: Default>(n: usize, test: bool, v: Result<&str, usize>, t: T, d: D, s: S) {
    let _: usize = n;
    let _: bool = test;
    let _: Result<&str, usize> = v;
    let _: T = t;
    let _: D = d;
    let _: S = s;
}

#[test]
fn test() {
    f(1, true, Ok("foo"), -1_i32, -1_i32, S);
}
