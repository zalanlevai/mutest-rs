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

```sh
cargo install --force --path mutest-driver
cargo install --force --path cargo-mutest
```

## Usage

> Currently, the invocation of the tool requires manually specifying the following values:
> * `MUTEST_SEARCH_PATH`: environment variable pointing to the local build artifacts, and
> * the compiler version used to build the tool.
>
> In addition, the Cargo test target may have to be specified explicitly with the `--lib` or `--bin <BIN>` options. For more targeting options, see `--help`.
>
> Thus, a typical invocation, with the code checked out and built in `~/Developer/mutest-rs` and a library target will look as follows:
>
> ```sh
> MUTEST_SEARCH_PATH=~/Developer/mutest-rs/target/debug cargo +nightly-2022-06-13 mutest --lib run
> ```

Run the `cargo mutest` subcommand against a standard Cargo package directory or workspace containing your crate.

```sh
cargo mutest -p <PACKAGE> run
```

See `--help` for more options and subcommands.
