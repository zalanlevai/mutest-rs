//@ build
//@ stderr: empty

#![feature(decl_macro)]

#![allow(unused)]

use std::io;

macro m() {
    fn test() -> io::Error {
        io::Error::new(io::ErrorKind::NotFound, "not found")
    }
}

m!();
