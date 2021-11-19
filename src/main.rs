include!("foo.rs");

mod foo;
mod foo_mutest;

fn main() {
    assert_eq!(foo::foo(5), 4);
    assert_eq!(foo_mutest::foo(5), 4);
}
