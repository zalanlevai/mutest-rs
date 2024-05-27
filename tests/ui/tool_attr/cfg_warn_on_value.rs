//@ build
//@ stderr
//@ rustc-flags: --check-cfg=cfg()

#![allow(unused)]

#[cfg(mutest = "foo")]
fn foo() {}

#[cfg_attr(mutest = "bar", mutest::skip)]
fn bar() {}
