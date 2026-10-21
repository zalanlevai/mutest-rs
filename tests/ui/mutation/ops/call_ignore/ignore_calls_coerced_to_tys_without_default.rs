//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_delete, call_value_default_shadow

#![allow(unused)]

trait Shape {}

#[derive(Default)]
struct Square(u32);

impl Shape for Square {}

#[mutest::skip]
fn boxed_square(side: u32) -> Box<Square> {
    Box::new(Square(side))
}

#[mutest::skip]
fn count(_: Box<dyn Shape>) -> usize {
    1
}

fn f() {
    // NOTE: These calls are coerced to `Box<dyn Shape>`, which does not implement `Default`.
    let _ = count(boxed_square(1));
    let _: Box<dyn Shape> = boxed_square(2);

    let _ = boxed_square(3);
}

#[test]
fn test() {
    f();
}
