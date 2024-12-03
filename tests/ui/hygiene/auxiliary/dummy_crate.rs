#![crate_type = "lib"]

pub fn foo() {}
pub fn bar() {}

pub struct S;
impl S {
    pub fn new() -> Self { S }
}
