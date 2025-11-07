//@ run: fail
//@ stdout
//@ stderr: empty
//@ no-harness
//@ bin

// NOTE: The `//@ no-harness` directive is required as the `--test` flag
//       overrides the `--crate-type=bin` argument.

fn main() {}

fn mutable_fn(_a: u32) { () }

#[test]
fn test() {
    mutable_fn(1);
}
