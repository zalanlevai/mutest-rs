//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ edition: 2024

#![feature(if_let_guard)]

#![allow(irrefutable_let_patterns)]

fn mutable_fn() {
    if let _ = true && false {}
    if false && false && let _ = true && false {}

    match () {
        _ if let _ = true && false => {}
        _ => {}
    }
    match () {
        _ if false && false && let _ = true && false => {}
        _ => {}
    }

    while let _ = true && false {}
    while false && false && let _ = true && false {}
}

#[test]
fn test() {
    mutable_fn();
}
