//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

struct ImplsDrop;

fn drop_impl() {}
impl Drop for ImplsDrop {
    fn drop(&mut self) {
        drop_impl();
    }
}

#[test]
fn test_implicit_drop_call() {
    let _ = ImplsDrop;
}

fn drop_impls_drop() {
    let _ = ImplsDrop;
}

#[test]
fn test_implicit_indirect_drop_call() {
    drop_impls_drop();
}

// NOTE: Same as `core::mem::drop`, but we explicitly define it for the sake of the test.
fn implicit_drop<T>(_: T) {}

#[test]
fn test_implicit_generic_drop_call() {
    implicit_drop(ImplsDrop);
}

struct NoDrop;

#[test]
fn test_no_implicit_drop_call() {
    let _ = NoDrop;
}
