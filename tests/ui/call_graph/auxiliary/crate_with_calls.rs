#![crate_type = "lib"]

pub trait ExternTrait {
    fn extern_trait_fn();
}

pub fn extern_fn_calling_trait_fn<T: ExternTrait>() {
    <T as ExternTrait>::extern_trait_fn();
}
