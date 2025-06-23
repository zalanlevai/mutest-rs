//@ build
//@ stderr
//@ mutest-flags: --depth=1 --call-graph-depth-limit=1

//! The following paths get normalized into paths that point to their trait items,
//! which means that the generic arguments in the original path segments will be discarded.
//! This is fine however, since we then add the qualified self type
//! from the type system, e.g. `<Vec<u8> as Default>::default`,
//! ensuring that resolution does not fail or change.
//!
//! This test is here to document this behavior and ensure that
//! paths like these will remain resolvable.

#![feature(decl_macro)]

macro m() {
    let _ = Vec::<u8>::default();
    let _ = std::collections::HashSet::<i32>::default();
}

#[test]
fn test() {
    m!();
}
