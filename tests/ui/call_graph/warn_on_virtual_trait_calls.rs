//@ print-targets
//@ stdout
//@ stderr

trait Trait {
    fn method(&self);
}

fn impl_method() {}
struct S;
impl Trait for S {
    fn method(&self) {
        impl_method();
    }
}

fn make_virtual_trait_call(v: Box<dyn Trait>) {
    v.method();
}

#[test]
fn test() {
    make_virtual_trait_call(Box::new(S));

    let v: &dyn Trait = &S;
    v.method();
}
