//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![feature(type_alias_impl_trait)]

#![allow(unused)]

// TEST: `impl Trait` in return ty position.
fn f_impl_trait_in_return_ty_position() -> impl Iterator<Item = usize> { 0.. }

// TEST: `impl Trait` in param ty position, which act as anonymous type parameters.
// TEST: `impl Trait` in param ty position, which act as anonymous type parameters.
fn f_impl_trait_in_param_ty_position(_msg: impl Into<String>) {}

// TEST: `impl Trait` ty alias.
type ImplTraitInTyAlias = impl Fn(usize) -> usize;
fn f_ty_alias_impl_trait_in_return_ty_position() -> ImplTraitInTyAlias { |v| v }
fn f_ty_alias_impl_trait_in_param_ty_position(_f: ImplTraitInTyAlias) {}

struct S;
impl S {
    // TEST: `impl Trait` in return ty position in inherent impl assoc function.
    fn f_impl_trait_in_return_ty_position() -> impl Iterator<Item = usize> { 0.. }

    // TEST: `impl Trait` in param ty position in inherent impl assoc function.
    fn f_impl_trait_in_param_ty_position(_msg: impl Into<String>) {}
}

trait Trait {
    // TEST: `impl Trait` in return ty position in trait assoc function.
    fn f_impl_trait_in_return_ty_position() -> impl Iterator<Item = usize>;

    // TEST: `impl Trait` in param ty position in trait assoc function.
    fn f_impl_trait_in_param_ty_position(_msg: impl Into<String>);
}

impl Trait for () {
    // TEST: `impl Trait` in return ty position in trait impl assoc function.
    fn f_impl_trait_in_return_ty_position() -> impl Iterator<Item = usize> { 0.. }

    // TEST: `impl Trait` in param ty position in trait impl assoc function.
    fn f_impl_trait_in_param_ty_position(_msg: impl Into<String>) {}
}
