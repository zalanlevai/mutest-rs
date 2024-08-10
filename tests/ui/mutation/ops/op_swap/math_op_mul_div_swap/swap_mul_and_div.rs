//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: math_op_mul_div_swap

fn f() {
    let _ = 1_u32 as usize * 5_usize;
    let _ = 3 * 6 * 0 * 1 * 0;

    let _ = 99 / 13_usize;
    let _ = 83 / 12 / 9;

    let _ = (131 / 22 * 1) as f64 * 0.1 / 99.0;

    if 0 * 1 > 0 {}

    match 33 / 2 {
        _ if 12 / 1 > 0 => {}
        _ => {}
    }

    for _ in 0..(19 / 3) {}
}

#[test]
fn test() {
    f();
}
