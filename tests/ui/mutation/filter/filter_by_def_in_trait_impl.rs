//@ print-targets
//@ print-mutations
//@ stdout
//@ stderr: empty
//@ mutest-flags: -v "--filter-mutations=def:<Vec2 as std::ops::Add>::add"

use std::ops::{Add, Sub};

#[derive(Copy, Clone, PartialEq, Debug)]
struct Vec2(f64, f64);

impl Add for Vec2 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

impl Sub for Vec2 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self(self.0 - other.0, self.1 - other.1)
    }
}

#[test]
fn test_vec2() {
    assert_eq!(Vec2(0.0, 0.0), Vec2(0.0, 0.0) + Vec2(0.0, 0.0));
    assert_eq!(Vec2(0.0, 0.0), Vec2(0.0, 0.0) - Vec2(0.0, 0.0));
}
