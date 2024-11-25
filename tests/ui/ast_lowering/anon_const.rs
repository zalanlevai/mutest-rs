//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

struct OneConstGenericArg<const N: usize>([u8; N]);

trait OneAssocConst {
    const N: i32;
}

#[test]
fn test() {
    // TEST: Anonymous const in enum variant.
    enum AnonConstInVariant { Variant = 1 }

    // TEST: Anonymous const literal as generic argument and as array length.
    let _ = OneConstGenericArg::<64>([0; 64]);

    // TEST: Anonymous const path as generic argument and as array length.
    const N: usize = 128;
    let _ = OneConstGenericArg::<N>([0; N]);
}
