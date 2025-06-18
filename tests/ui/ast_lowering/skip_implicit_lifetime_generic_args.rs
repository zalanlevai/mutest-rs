//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

#[test]
fn test() {
    // TEST: Generic args with implicit lifetime generics.
    trait LifetimeAndTypeGenerics<'a, 'b, T> {
        type Type;
    }
    impl<'a, 'b> LifetimeAndTypeGenerics<'a, 'b, i32> for () {
        type Type = u8;
    }
    let _: <() as LifetimeAndTypeGenerics<i32>>::Type = 0;
}
