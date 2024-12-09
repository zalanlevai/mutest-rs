#![crate_type = "lib"]

#![feature(decl_macro)]

pub fn foo() {}
pub fn bar() {}

pub struct S;
impl S {
    pub fn new() -> Self { S }
}

pub macro use_foo_from_crate_root_through_macro_expansion() {
    use crate::foo;
}
