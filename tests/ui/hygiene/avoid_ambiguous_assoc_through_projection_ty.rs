//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    trait Error {
        fn duplicate_field() -> Self;
    }

    trait MapAccess {
        type Error: Error;
    }

    fn visit_map<__A: MapAccess>() -> Result<(), __A::Error> {
        Err(<__A::Error as Error>::duplicate_field())
    }
}

m!();
