//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait Error {}

    trait EnumAccess {
        type Error: Error;
        fn variant<V>(self) -> Result<V, Self::Error>;
    }

    fn visit_enum<__A: EnumAccess>(__data: __A) -> Result<(), __A::Error> {
        type __Field = ();
        Result::map(EnumAccess::variant::<__Field>(__data), |v| v)
    }
}

#[test]
fn test() {
    m!();
}
