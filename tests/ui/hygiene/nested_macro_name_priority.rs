//@ build
//@ stderr: empty

//! It seems that currently, whether intentionally or otherwise,
//! names in a nested macro invocation can be introduced without conflict,
//! but when referencing names in the same macro,
//! names defined by the top-level macro silently take priority
//! over ones defined locally.
//!
//! This behavior seems to be specific to nested macro definitions/invocations.

#![feature(decl_macro)]

macro m() {
    macro a() {
        macro b() {
            macro c() {
                #[allow(unused)] fn f() -> usize { 4 }
                assert_eq!(-1, f());
            }
            c!();
            #[allow(unused)] fn f() -> u32 { 3 }
            assert_eq!(-1, f());
        }
        b!();
        #[allow(unused)] fn f() -> u8 { 2 }
        assert_eq!(-1, f());
    }
    a!();
    #[allow(unused)] fn f() -> i32 { -1 }
    assert_eq!(-1, f());
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_nested_macro_name_priority() {
        crate::m!();
    }
}
