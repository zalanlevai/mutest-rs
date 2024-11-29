//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

#[test]
fn test() {
    let mut a: u32;

    let () = ();
    let a: i8 = 1;

    let Some(_) = Some(1) else {
        return;
    };
}
