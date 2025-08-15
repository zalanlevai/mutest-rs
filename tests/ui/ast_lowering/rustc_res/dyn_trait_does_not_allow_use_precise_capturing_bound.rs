//@ build: fail
//@ stderr

#![allow(unused)]

trait DynTrait<'a, 'b, 'c> {}

fn dyn_existential<'a, 'b, 'c, 'x, 'y>() -> Option<Box<dyn DynTrait<'a, 'b, 'c> + 'x + use<'y>>> {
    None
}
