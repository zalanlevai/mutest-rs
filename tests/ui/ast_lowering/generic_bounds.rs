//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

#[test]
fn test() {
    trait Trait {}

    struct ImplTrait;
    impl Trait for ImplTrait {}

    fn fn_returning_impl_trait_with_precise_capturing_args<'a, T>() -> impl Trait + use<'a, T> { ImplTrait }
}
