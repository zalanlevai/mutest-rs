//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

use std::sync::atomic::{AtomicBool, AtomicI32, AtomicUsize};

macro m() {
    fn test() {
        let _ = AtomicBool::new(false);
        let _ = AtomicI32::new(0);
        let _ = AtomicUsize::new(0);
    }
}

m!();
