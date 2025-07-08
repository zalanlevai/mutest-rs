//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait Trait {}
    struct S;
    impl Trait for S {}

    /// TEST: The `dyn` trait object type's syntax is ambiguous unless it is surrounded by parentheses.
    ///       The `dyn` type needs to be in a generic argument position to trigger hygienic type printing.
    fn test() -> Box<&'static (dyn Trait + Send)> {
        Box::new(&S)
    }
}

m!();
