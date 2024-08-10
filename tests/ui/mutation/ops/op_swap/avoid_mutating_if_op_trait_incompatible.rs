//@ print-mutants
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: math_op_add_sub_swap

use std::ops::{Add, Sub};

struct DifferentRhs(usize);

impl Add for DifferentRhs {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

impl Sub<usize> for DifferentRhs {
    type Output = Self;

    fn sub(self, other: usize) -> Self {
        Self(self.0 - other)
    }
}

struct DifferentOutput(usize);

impl Add for DifferentOutput {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

impl Sub for DifferentOutput {
    type Output = usize;

    fn sub(self, other: Self) -> Self::Output {
        self.0 - other.0
    }
}

fn f() {
    let _ = DifferentRhs(1) + DifferentRhs(2);
    let _ = DifferentOutput(4) - DifferentOutput(3);
}

#[test]
fn test() {
    f();
}
