//@ build
//@ verify: ast-lowering
//@ stderr: empty

#[test]
fn test() {
    match 1 {
        std::os::raw::c_int::MIN..0 => {}
        0 => {}
        1..=std::os::raw::c_int::MAX => {}
    }
}
