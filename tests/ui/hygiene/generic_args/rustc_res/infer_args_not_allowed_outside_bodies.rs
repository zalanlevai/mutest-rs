//@ rustc
//@ build: fail
//@ stderr

//! This test shows that `_` infer arguments cannot be used outside of bodies.

#![allow(unused)]

trait Trait<T> {
    type Assoc;
}

impl Trait<()> for u8 {
    type Assoc = u8;
}

// TEST: Function return type.
fn ret_ty() -> <u8 as Trait<_>>::Assoc { 0 }

// TEST: Funtion parameter type.
fn param_ty(_: <u8 as Trait<_>>::Assoc) {}

// TEST: Where clause.
fn where_clause<T>() where T: Trait<<u8 as Trait<_>>::Assoc> {}

// TEST: Struct field type.
struct Struct {
    field: <u8 as Trait<_>>::Assoc,
}

// TEST: Const item type.
const CONST: <u8 as Trait<_>>::Assoc = 0;

// TEST: Impl header.
impl Trait<<u8 as Trait<_>>::Assoc> for u8 {
    type Assoc = ();
}

// TEST: Associated type.
impl Trait<u32> for () {
    type Assoc = <u8 as Trait<_>>::Assoc;
}
