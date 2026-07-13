//@ build
//@ verify: ast-lowering
//@ stderr: empty

#![allow(unused)]

trait Trait {
    type AssocTy<T>;
}

impl Trait for () {
    type AssocTy<T> = ();
}
