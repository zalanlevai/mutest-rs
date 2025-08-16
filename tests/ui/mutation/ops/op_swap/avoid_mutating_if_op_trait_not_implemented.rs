//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: math_op_add_sub_swap, math_op_mul_div_swap

use std::ops::{Add, Div, Mul, MulAssign};

struct S(usize);

impl Add for S {
    type Output = Self;

    #[mutest::skip]
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}

impl Mul for S {
    type Output = Self;

    #[mutest::skip]
    fn mul(self, other: Self) -> Self {
        Self(self.0 * other.0)
    }
}

impl MulAssign for S {
    #[mutest::skip]
    fn mul_assign(&mut self, other: Self) {
        *self = Self(self.0 * other.0);
    }
}

impl Div for S {
    type Output = Self;

    #[mutest::skip]
    fn div(self, other: Self) -> Self {
        Self(self.0 / other.0)
    }
}

fn f() {
    let mut s = S(1) + S(2);

    s *= S(3);
}

#[test]
fn test() {
    f();
}
