//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: logical_op_and_or_swap

fn f() {
    let _ = (1_u32 as usize != 0) && (5_usize == 1);
    let _ = true && false && false && 1.0_f32 == 1 as f32;

    let _ = (99 != 0) || (13_usize == 1);
    let _ = true || false || false;

    let not_a_bool = 5 + 7 - 3;

    let _ = (not_a_bool + 1) == 10 || not_a_bool * 0 >= 1 && (true || false);

    if (false && true) as u32 > 0 {}

    match (33 > 1) || (2 == 2) {
        _ if (12 == 1 || not_a_bool > 0) as i8 > 0 => {}
        _ => {}
    }

    for _ in 0..((false || true) as usize) {}
}

#[test]
fn test() {
    f();
}
