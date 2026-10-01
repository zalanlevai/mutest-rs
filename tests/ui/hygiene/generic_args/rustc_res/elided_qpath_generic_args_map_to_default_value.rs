//@ rustc
//@ build
//@ stderr: empty

//! This test shows that elided generic args in qualified paths
//! resolve to the default value of the corresponding generic parameters,
//! and are not considered ambiguous in the presence of
//! competing impls with differing generic args.

#![allow(unused)]

struct A;
struct B;

trait Trait<M = ()> {
    const ASSOC_CONST: usize;
}

impl Trait for u8 {
    const ASSOC_CONST: usize = 0;
}
impl Trait<A> for u8 {
    const ASSOC_CONST: usize = 1;
}
impl Trait<B> for u8 {
    const ASSOC_CONST: usize = 2;
}

const _: [(); 0] = [(); <u8 as Trait>::ASSOC_CONST];
const _: [(); 0] = [(); <u8 as Trait<>>::ASSOC_CONST];
const _: [(); 1] = [(); <u8 as Trait<A>>::ASSOC_CONST];
const _: [(); 2] = [(); <u8 as Trait<B>>::ASSOC_CONST];
