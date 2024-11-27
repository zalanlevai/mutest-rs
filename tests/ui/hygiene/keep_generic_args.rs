//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    pub struct AnyValueId {
        type_id: std::any::TypeId,
        type_name: &'static str,
    }

    impl AnyValueId {
        pub fn of<A: ?Sized + 'static>() -> Self {
            Self {
                type_id: std::any::TypeId::of::<A>(),
                type_name: std::any::type_name::<A>(),
            }
        }
    }

    fn f() {
        let _ = AnyValueId::of::<i32>();
    }
}

m!();
