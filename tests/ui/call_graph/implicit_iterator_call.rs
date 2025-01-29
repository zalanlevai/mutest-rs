//@ print-targets
//@ stdout
//@ stderr: empty

fn next_impl() {}

struct ImplsIterator;

impl Iterator for ImplsIterator {
    type Item = ();

    fn next(&mut self) -> Option<Self::Item> {
        next_impl();
        None
    }
}

#[test]
fn test() {
    let iterator = ImplsIterator;
    for _ in iterator {}
}
