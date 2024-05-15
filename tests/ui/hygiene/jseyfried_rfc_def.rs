//@ build
//@ stderr: empty
//@ mutest-flags: --Zsanitize-macro-expns

//! This test is based on the linked RFC text on macros 2.0 hygiene in the
//! initial implementation PR at https://github.com/rust-lang/rust/pull/40847,
//! see the RFC for the second of the two examples tested here
//! ("Reliably define and use items/fields/lifetimes/etc. in a macro definition"):
//! https://github.com/jseyfried/rfcs/blob/hygiene/text/0000-hygiene.md.

#![feature(decl_macro)]

#![allow(unused)]

struct Resolution<const IDX: isize>;

macro m($f:ident, $T:ident, $x:ident) {
    fn f() -> crate::Resolution<0> { crate::Resolution } // (i) This is never a conflict error
    let _: crate::Resolution<0> = f(); // This always resolves to (i)

    mod foo {
        pub fn $f() -> crate::Resolution<1> { crate::Resolution } // (i)
        pub fn f() -> crate::Resolution<2> { crate::Resolution } // (ii) This is never a conflict error
    }
    let _: crate::Resolution<1> = foo::$f(); // This always resolves to (i)
    let _: crate::Resolution<2> = foo::f(); // This always resolves to (ii)

    fn g<$T, T /* this is never a conflict error */>(x: ($T, T)) {}

    struct S {
        $x: u32,
        x: i32, // This is never a conflict error
    }

    impl S {
        fn $f(&self) -> u32 { 0 }
        fn f(&self) -> i32 { 0 } // This is never a conflict error
    }

    let s = S { x: 0, $x: 0 };
    let _: (u32, i32) = (s.$x, s.x); // This always has type (u32, i32)
    let _: (u32, i32) = (s.$f(), s.f()); // This always has type (u32, i32)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test() {
        crate::m!(f, T, x);
    }
}
