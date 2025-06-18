//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

use std::sync::mpsc;
use std::time::Duration;

macro m() {
    fn test() {
        let (_tx, rx) = mpsc::channel::<()>();

        match rx.recv_timeout(Duration::ZERO) {
            Err(mpsc::RecvTimeoutError::Timeout) => {}
            _ => {}
        }
    }
}

m!();
