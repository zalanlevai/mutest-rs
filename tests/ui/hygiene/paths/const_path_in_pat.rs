//@ build
//@ stderr: empty

#![feature(decl_macro)]

macro m() {
    match 1 {
        std::os::raw::c_int::MIN..0 => {}
        0 => {}
        1..=std::os::raw::c_int::MAX => {}
    }
}

#[test]
fn test() {
    m!();
}
