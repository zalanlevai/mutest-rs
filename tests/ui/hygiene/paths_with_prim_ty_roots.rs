//@ build
//@ stderr: empty

#![feature(decl_macro)]
#![feature(f16)]
#![feature(f128)]

macro m() {
    // Paths to assoc items in inherent impls for primitive types.
    let _ = bool::then(true, || ());
    let _ = char::from_u32(0);
    let _ = i8::reverse_bits(5);
    let _ = i16::ilog2(10);
    let _ = i32::from_str_radix("1234", 16);
    let _ = i64::leading_zeros(987);
    let _ = i128::wrapping_add(1, 2);
    let _ = isize::MAX;
    let _ = u8::reverse_bits(5);
    let _ = u16::ilog2(10);
    let _ = u32::from_str_radix("1234", 16);
    let _ = u64::leading_zeros(987);
    let _ = u128::wrapping_add(1, 2);
    let _ = usize::MAX;
    let _ = f16::EPSILON;
    let _ = f32::abs(-1.0);
    let _ = f64::signum(0.0);
    let _ = f128::to_bits(0.1);
    let _ = str::len("abc");
}

#[test]
fn test() {
    m!();
}
