# mutest-rs &mdash; Mutation testing tools for Rust

Generate and analyze runtime-swappable code mutants of Rust programs using a dynamic set of abstract mutation operators.

## Build

> `mutest` relies on the nightly compiler toolchain. `rustup` is configured to automatically install and use the right nightly version.

The `mutest-runtime` crate, injected into the generated code must be built separately. This builds an `rlib` file which is linked against by the compiler.

```sh
cargo build -p mutest-runtime
```

Build the rest of the project as usual.

```sh
cargo build -p mutest
```

## Usage

Run the `mutest` bin against a standard Cargo package directory containing your crate.

```sh
cargo run -p mutest -- <package_dir>
```
