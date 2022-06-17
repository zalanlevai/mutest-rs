# mutest-rs &mdash; Mutation testing tools for Rust

Generate and analyze runtime-swappable code mutants of Rust programs using a dynamic set of abstract mutation operators.

![Output of mutest](docs/res/output.png)

## Build

> `mutest` relies on the nightly compiler toolchain. `rustup` is configured to automatically install and use the right nightly version.

Build the workspace using Cargo.

```sh
cargo clean
cargo build
```

Install `mutest-driver` and the Cargo subcommand `cargo-mutest` locally.

> The `mutest-runtime` crate, injected into the generated code is currently assumed to be in `./target/debug`.

```sh
cargo install --force --path mutest-driver
cargo install --force --path cargo-mutest
```

## Usage

Run the `cargo mutest` subcommand against a standard Cargo package directory or workspace containing your crate.

```sh
cargo mutest -p <PACKAGE> run
```

See `--help` for more options and subcommands.
