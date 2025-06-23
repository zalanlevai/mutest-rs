//@ print-call-graph
//@ print-targets
//@ stdout
//@ stderr: empty

struct ImplsEq;

fn eq_impl() {}
fn ne_impl() {}

impl PartialEq for ImplsEq {
    fn eq(&self, _other: &Self) -> bool {
        eq_impl();
        true
    }

    fn ne(&self, _other: &Self) -> bool {
        ne_impl();
        false
    }
}

impl Eq for ImplsEq {}

#[test]
fn test_implicit_eq_call() {
    let _ = ImplsEq == ImplsEq;
}

#[test]
fn test_implicit_ne_call() {
    let _ = ImplsEq != ImplsEq;
}
