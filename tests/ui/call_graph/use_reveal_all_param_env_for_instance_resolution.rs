//@ print-call-graph
//@ stderr: empty
//@ edition: 2021

//! Since we are performing monomorphising call graph construction after type checking,
//! we do so by querying monomorphic obligations.
//! To ensure that default trait implementations are revealed by resolution,
//! we must use parameter environments which reveal all definitions.
//! See `rustc_ty_utils::instance::resolve_associated_item` for more information.

fn opaque_iterator() -> impl Iterator<Item = i32> {
    [1, 2, 3].into_iter()
}

#[test]
fn test() {
    let _ = opaque_iterator().collect::<Vec<_>>();
}
