//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

trait NoAssocTypes {}

trait OneAssocType {
    type A;
}

trait TwoAssocTypes {
    type A;
    type B;
}

#[test]
fn test() {
    // Equality constraints
    type OneEqConstraint = dyn OneAssocType<A = i32>;
    type TwoEqConstraints = dyn TwoAssocTypes<A = &'static str, B = Option<usize>>;
    type NestedEqConstraints = dyn TwoAssocTypes<A = Option<i8>, B = Box<dyn OneAssocType<A = f32>>>;

    // Bound constraints
    trait OneBoundConstraint<T: OneAssocType<A: NoAssocTypes>> {}
    trait TwoBoundConstraints<T: TwoAssocTypes<A: NoAssocTypes, B: NoAssocTypes>> {}
    trait NestedBoundConstraints<T: TwoAssocTypes<A: NoAssocTypes, B: TwoAssocTypes<A: NoAssocTypes, B: OneAssocType<A: NoAssocTypes>>>> {}

    // Mixed constraints
    trait MixedConstraints<T: TwoAssocTypes<A = Option<&'static str>, B: TwoAssocTypes<A: NoAssocTypes, B = i16>>> {}
}
