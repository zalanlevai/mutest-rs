//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: math_op_add_sub_swap

use std::time::{Duration, Instant};

fn f() {
    let duration = Instant::now() - Instant::now();

    let _ = Instant::now() + duration;
    let _ = Instant::now() - Duration::from_secs(3);
}

#[test]
fn test() {
    f();
}
