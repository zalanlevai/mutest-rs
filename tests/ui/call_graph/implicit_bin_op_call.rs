//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

#![feature(decl_macro)]

macro test($test:ident, $ty:ident, $op_trait:path|$op_assign_trait:path, $op_fn:ident|$op_assign_fn:ident, $op_impl_fn:ident|$op_assign_impl_fn:ident, $op:tt|$op_assign:tt) {
    struct $ty;

    fn $op_impl_fn() {}
    impl $op_trait for $ty {
        type Output = Self;

        fn $op_fn(self, _rhs: Self) -> Self::Output {
            $op_impl_fn();
            Self
        }
    }

    fn $op_assign_impl_fn() {}
    impl $op_assign_trait for $ty {
        fn $op_assign_fn(&mut self, _rhs: Self) {
            $op_assign_impl_fn();
            *self = Self;
        }
    }

    #[test]
    fn $test() {
        let mut v = $ty $op $ty;
        v $op_assign $ty;
    }
}

test!(test_implicit_add_call, ImplsAdd, ::std::ops::Add|::std::ops::AddAssign, add|add_assign, add_impl|add_assign_impl, + | +=);
test!(test_implicit_sub_call, ImplsSub, ::std::ops::Sub|::std::ops::SubAssign, sub|sub_assign, sub_impl|sub_assign_impl, - | -=);
test!(test_implicit_mul_call, ImplsMul, ::std::ops::Mul|::std::ops::MulAssign, mul|mul_assign, mul_impl|mul_assign_impl, * | *=);
test!(test_implicit_div_call, ImplsDiv, ::std::ops::Div|::std::ops::DivAssign, div|div_assign, div_impl|div_assign_impl, / | /=);
test!(test_implicit_rem_call, ImplsRem, ::std::ops::Rem|::std::ops::RemAssign, rem|rem_assign, rem_impl|rem_assign_impl, % | %=);
test!(test_implicit_bit_or_call, ImplsBitOr, ::std::ops::BitOr|::std::ops::BitOrAssign, bitor|bitor_assign, bitor_impl|bitor_assign_impl, | | |=);
test!(test_implicit_bit_xor_call, ImplsBitXor, ::std::ops::BitXor|::std::ops::BitXorAssign, bitxor|bitxor_assign, bitxor_impl|bitxor_assign_impl, ^ | ^=);
test!(test_implicit_bit_and_call, ImplsBitAnd, ::std::ops::BitAnd|::std::ops::BitAndAssign, bitand|bitand_assign, bitand_impl|bitand_assign_impl, & | &=);
test!(test_implicit_shl_call, ImplsShl, ::std::ops::Shl|::std::ops::ShlAssign, shl|shl_assign, shl_impl|shl_assign_impl, << | <<=);
test!(test_implicit_shr_call, ImplsShr, ::std::ops::Shr|::std::ops::ShrAssign, shr|shr_assign, shr_impl|shr_assign_impl, >> | >>=);
