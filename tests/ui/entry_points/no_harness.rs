//@ run: fail
//@ stdout
//@ stderr: empty
//@ no-harness

fn mutable_fn(_a: u32) { () }

#[test]
fn test() {
    mutable_fn(1);
}
