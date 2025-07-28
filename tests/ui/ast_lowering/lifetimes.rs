//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

struct Struct<'a, 'b, 'c> {
    _phantom: (&'a (), &'b (), &'c ()),
}

trait Trait<'a, 'b, 'c> {
    type AssocTy<'x, 'y>;

    fn assoc_fn<'x, 'y, 'z>(_: &'z ()) -> Self::AssocTy<'x, 'y>;
}

impl<'a, 'b, 'c> Trait<'a, 'b, 'c> for &&Struct<'a, 'b, 'c>
where
    'a: 'b + 'c,
{
    type AssocTy<'x, 'y> = ();

    fn assoc_fn<'x, 'y, 'z>(_: &'z ()) -> Self::AssocTy<'x, 'y> {}
}

impl Trait<'_, '_, '_> for &'_ &&'_ () {
    type AssocTy<'x, 'y> = ();

    fn assoc_fn<'x, 'y, 'z>(_: &'z ()) -> Self::AssocTy<'x, 'y> {}
}

trait DynTrait<'a, 'b, 'c> {}
impl DynTrait<'_, '_, '_> for () {}

fn dyn_existential<'a, 'b, 'c, 'x>() -> Option<Box<dyn DynTrait<'a, 'b, 'c> + 'x>> {
    None
}

fn impl_trait<'a, 'b, 'c, 'x, 'y>() -> Option<impl DynTrait<'a, 'b, 'c> + 'x + use<'a, 'b, 'c, 'x, 'y>> {
    None::<()>
}
