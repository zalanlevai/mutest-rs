//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

#![feature(decl_macro)]

#![allow(unused)]

macro m() {
    fn local_item_in_fn() {
        struct A;
        struct S { a: A }
        trait I {
            fn f(&self) -> &A { &A }
        }
        impl S {
            fn f(&self) -> &A { &self.a }
        }
    }

    const LOCAL_ITEM_IN_CONST: () = {
        struct A;
        struct S { a: A }
        trait I {
            fn f(&self) -> &A { &A }
        }
        impl S {
            fn f(&self) -> &A { &self.a }
        }
    };

    fn item_in_nested_parent_fn() {
        struct A;

        fn f() {
            struct S { a: A }
            trait I {
                fn f(&self) -> &A { &A }
            }
            impl S {
                fn f(&self) -> &A { &self.a }
            }
        }
    }

    fn item_outside_foreign_mod() {
        #[repr(C)] struct A { a: u32 }

        extern "C" {
            fn f() -> A;
        }
    }
}

#[test]
fn test() {
    m!();
}
