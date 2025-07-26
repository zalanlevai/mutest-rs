//@ build
//@ verify: ast_lowering
//@ stderr: empty

#![allow(unused)]

trait Trait {
    type AssocTy<T>;
}

impl Trait for () {
    type AssocTy<T> = ();
}
