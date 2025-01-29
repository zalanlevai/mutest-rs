//@ print-targets
//@ stdout
//@ stderr: empty

fn clone_impl() {}

struct ImplsClone;

impl Clone for ImplsClone {
    fn clone(&self) -> Self {
        clone_impl();
        Self
    }
}

#[test]
fn test() {
    let _ = ImplsClone.clone();
}
