//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_value_default_shadow
//@ aux-build: private_dep.rs
//@ aux-build: non_reexporting_proxy.rs

#![allow(unused)]

extern crate non_reexporting_proxy;

fn f() {
    let _ = non_reexporting_proxy::proxy_struct(1);

    // NOTE: The crate of the returned type is not reachable from this crate, so the type cannot be named.
    let _ = non_reexporting_proxy::private_dep_struct(1);
}

#[test]
fn test() {
    f();
}
