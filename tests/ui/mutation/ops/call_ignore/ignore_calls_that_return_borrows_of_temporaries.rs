//@ print-mutations
//@ build
//@ stdout
//@ stderr: empty
//@ mutation-operators: call_delete, call_value_default_shadow

#![allow(unused)]

struct Wrapper(String);

impl Wrapper {
    fn inner(&self) -> &str { &self.0 }
}

#[mutest::skip]
fn wrap(s: &str) -> Wrapper {
    Wrapper(s.to_owned())
}

#[mutest::skip]
fn wrapper_ref(wrapper: &Wrapper) -> &Wrapper {
    wrapper
}

const EMPTY: String = String::new();
static STATIC_EMPTY: String = String::new();

fn first<'a>(s: &'a String, _: ()) -> &'a str {
    s.as_str()
}

fn f(input: &[u8]) {
    // NOTE: These calls return borrows of temporaries created within the call.
    let _ = String::from_utf8_lossy(input).trim().len();
    let _ = wrap("a").inner().len();
    let _ = first(&String::new(), ()).len();
    // NOTE: These calls return borrows of fields of temporaries.
    let _ = first(&wrap("b").0, ()).len();
    let _ = wrap("c").0.as_str().len();
    // NOTE: Each use of a constant creates a new temporary.
    let _ = first(&EMPTY, ()).len();
    let _ = EMPTY.as_str().len();

    // NOTE: These calls only borrow locals, statics, or places behind references.
    let s = String::new();
    let _ = s.trim().len();
    let _ = first(&s, ()).len();
    let w = wrap("d");
    let _ = first(&wrapper_ref(&w).0, ()).len();
    let _ = wrapper_ref(&w).0.as_str().len();
    let _ = first(&STATIC_EMPTY, ()).len();
    let _ = STATIC_EMPTY.as_str().len();
}

#[test]
fn test() {
    f(b"");
}
