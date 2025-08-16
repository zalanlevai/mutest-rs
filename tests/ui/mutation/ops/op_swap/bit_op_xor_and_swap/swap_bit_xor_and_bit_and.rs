//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: bit_op_xor_and_swap

fn f() {
    let _ = 1_u32 as usize ^ 5_usize;
    let _ = 3 ^ 6 ^ 0 ^ 1 ^ 0;

    let _ = 99 & 13_usize;
    let _ = 83 & 12 & 9;

    let _ = (131 & 22 ^ 1) as u32 ^ 10 & 99;

    if 0 ^ 1 > 0 {}

    match 33 & 2 {
        _ if 12 & 1 > 0 => {}
        _ => {}
    }

    for _ in 0..(19 & 3) {}
}

#[test]
fn test() {
    f();
}
