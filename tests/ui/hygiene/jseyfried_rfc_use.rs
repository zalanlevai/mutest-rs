//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

//! This test is based on the linked RFC text on macros 2.0 hygiene in the
//! initial implementation PR at https://github.com/rust-lang/rust/pull/40847,
//! see the RFC for the first of the two examples tested here
//! ("Reliably use items accessible from the macro definition"):
//! https://github.com/jseyfried/rfcs/blob/hygiene/text/0000-hygiene.md.

#![feature(decl_macro)]

struct Resolution<const IDX: isize>;

fn f() -> crate::Resolution<1> { crate::Resolution } // (i)
pub mod foo {
    pub(super) fn g() -> crate::Resolution<2> { crate::Resolution } // (ii)
    pub(super) mod bar {
        pub(crate) fn h() -> crate::Resolution<3> { crate::Resolution } // (iii)
    }
    pub(super) trait T {
        fn f(&self) -> crate::Resolution<4> { crate::Resolution } // (iv)
    }
    impl T for () {}

    pub macro m() {
        use super::f; // This always imports (i)
        let _: crate::Resolution<1> = f();

        let _: crate::Resolution<1> = super::f(); // This always resolves to (i)

        let _: crate::Resolution<2> = g(); // This always resolves to (ii)

        use self::bar::*; // This always imports (iii)
        let _: crate::Resolution<3> = h(); // This always resolves to (iii)

        use crate::foo::T;
        let _: crate::Resolution<4> = ().f(); // This method call always resolves to (iv)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        crate::foo::m!();
    }
}
