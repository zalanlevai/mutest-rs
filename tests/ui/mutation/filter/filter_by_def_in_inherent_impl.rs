//@ print-targets
//@ print-mutations
//@ stdout
//@ stderr: empty
//@ mutest-flags: -v --filter-mutations=def:Vec2::len

#[derive(Copy, Clone)]
struct Vec2(f64, f64);

impl Vec2 {
    fn dot(self, other: Vec2) -> f64 {
        (self.0 * other.0) + (self.1 * other.1)
    }

    fn len(self) -> f64 {
        f64::sqrt(self.dot(self))
    }
}

#[test]
fn test_vec2() {
    assert_eq!(0.0, Vec2(0.0, 0.0).dot(Vec2(0.0, 0.0)));
    assert_eq!(0.0, Vec2(0.0, 0.0).len());
}
