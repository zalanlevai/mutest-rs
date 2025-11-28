//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]
trait TraitWithGenericAssociatedType {
    type Type<'a>
    where
        Self: 'a;
}

// TEST: Second where clause in type alias after type.
impl TraitWithGenericAssociatedType for () {
    type Type<'a> = ()
    where
        Self: 'a;
}
