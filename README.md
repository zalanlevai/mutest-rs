# mutest-rs &mdash; Mutation testing tools for Rust

[![DOI 10.1109/ICST57152.2023.00014](https://img.shields.io/badge/10.1109%2FICST57152.2023.00014-black?logo=DOI)](https://doi.org/10.1109/ICST57152.2023.00014)

Generate and analyze runtime-swappable code mutants of Rust programs using a dynamic set of abstract mutation operators.

![Output of mutest](docs/res/output.png)

> [!NOTE]
> mutest-rs is primarily a research tool, but is an extremely capable mutation testing tool for all use cases. It was originally developed for my ongoing PhD work.

## Build

> `mutest` relies on the nightly compiler toolchain. `rustup` is configured to automatically install and use the right nightly version.

Build the `mutest-runtime` crate in release mode.

```sh
cargo build --release -p mutest-runtime
```

Install `mutest-driver` and the Cargo subcommand `cargo-mutest` locally.

```sh
cargo install --force --path mutest-driver
cargo install --force --path cargo-mutest
```

> The release build of `mutest-driver` looks for a release build of `mutest-runtime`. A local install with `cargo install` will produce a release build.

## Usage

> [!IMPORTANT]
> Currently, the invocation of the tool requires manually specifying the following values:
> * `MUTEST_SEARCH_PATH`: environment variable pointing to the local build artifacts, and
> * the compiler version used to build the tool.
>
> In addition, the Cargo test target may have to be specified explicitly with the `--lib` or `--bin <BIN>` options. For more targeting options, see `--help`.
>
> Thus, a typical invocation, with the code checked out and built in `~/Developer/mutest-rs` and a library target will look as follows:
>
> ```sh
> export MUTEST_SEARCH_PATH=~/Developer/mutest-rs/target/release
> cargo +nightly-2023-09-26 mutest --lib run
> ```

Run the `cargo mutest` subcommand against a standard Cargo package directory or workspace containing your crate.

```sh
cargo mutest -p <PACKAGE> run
```

See `--help` for more options and subcommands.

## License

The mutest-rs project is dual-licensed under Apache 2.0 and MIT terms.

See [LICENSE-APACHE](LICENSE-APACHE), [LICENSE-MIT](LICENSE-MIT), and [COPYRIGHT](COPYRIGHT) for details.
