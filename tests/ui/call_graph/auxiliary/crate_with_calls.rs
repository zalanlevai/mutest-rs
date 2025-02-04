#![crate_type = "lib"]

pub trait ExternTrait {
    fn extern_trait_fn();
}

fn extern_private_fn() {}

pub fn extern_fn_calling_trait_fn<T: ExternTrait>() {
    <T as ExternTrait>::extern_trait_fn();

    // NOTE: This function call is here to represent any calls in extern functions that point to
    //       functions with no encoded MIR form available (e.g. private, non-generic functions)
    //       See `rustc_metadata::rmeta::encoder::should_encode_mir` for more details.
    extern_private_fn();
}
