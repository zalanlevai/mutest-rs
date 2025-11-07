//@ run: fail
//@ stdout
//@ stderr: empty

fn mutable_fn(_a: u32) { () }

#[test]
fn test() {
    mutable_fn(1);
}
