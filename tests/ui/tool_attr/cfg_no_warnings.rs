//@ build
//@ stderr: empty
//@ rustc-flags: --check-cfg=cfg()

#![allow(unused)]

#[cfg(mutest)]
#[cfg_attr(mutest, mutest::skip)]
fn foo() {}
