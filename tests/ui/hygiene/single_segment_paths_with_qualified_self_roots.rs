//@ build
//@ stderr: empty

#![feature(inherent_associated_types)]
#![allow(incomplete_features)]

#![allow(unused)]

trait HasError {
    type Error;
}

trait GenericHasError<E> {
    type Error;
}

// NOTE: These are the more common formulations of the single-segment paths below.
fn return_two_segment_path_with_trait_generic_root<S: HasError>() -> Result<(), S::Error> {
    Ok(())
}
fn return_two_segment_path_with_generic_trait_generic_root<S: GenericHasError<()>>() -> Result<(), S::Error> {
    Ok(())
}

fn return_single_segment_path_with_trait_qualified_self_root<S: HasError>() -> Result<(), <S>::Error> {
    Ok(())
}
fn return_single_segment_path_with_generic_trait_qualified_self_root<S: GenericHasError<()>>() -> Result<(), <S>::Error> {
    Ok(())
}

struct StructHasType;
impl StructHasType {
    type Type = ();
}

fn return_single_segment_path_with_struct_qualified_self_root() -> Result<(), <StructHasType>::Type> {
    Ok(())
}
