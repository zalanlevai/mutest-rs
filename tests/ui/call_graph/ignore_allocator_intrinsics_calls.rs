//@ print-call-graph
//@ stderr: empty

use std::alloc::Layout;

fn make_allocator_intrinsics_call() {
    let _ = unsafe { std::alloc::alloc(Layout::from_size_align_unchecked(8, 8)) };
}

#[test]
fn test() {
    make_allocator_intrinsics_call();
}
