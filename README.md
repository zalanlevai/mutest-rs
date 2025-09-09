# mutest-rs &mdash; Mutation testing tools for Rust

[![Docs](https://img.shields.io/badge/mutest.rs-black?logo=mdbook)](https://mutest.rs)
[![DOI 10.1109/ICST57152.2023.00014](https://img.shields.io/badge/10.1109%2FICST57152.2023.00014-black?logo=DOI)](https://doi.org/10.1109/ICST57152.2023.00014)

Generate and analyze runtime-swappable code mutants of Rust programs using a dynamic set of abstract mutation operators. For more information, see [the mutest-rs book](https://mutest.rs).

![Output of mutest](docs/res/output.png)

> [!NOTE]
> mutest-rs is primarily a research tool, but is an extremely capable mutation testing tool for all use cases. It was originally developed for my ongoing PhD work.

## Mutation Operators

Currently, the following list of mutation operators are implemented:

| Mutation Operator           | Short Description                                                      |
| --------------------------- | ---------------------------------------------------------------------- |
| `arg_default_shadow`        | Ignore argument by shadowing it with `Default::default()`.             |
| `bit_op_or_and_swap`        | Swap bitwise OR for bitwise AND and vice versa.                        |
| `bit_op_or_xor_swap`        | Swap bitwise OR for bitwise XOR and vice versa.                        |
| `bit_op_shift_dir_swap`     | Swap the direction of bitwise shift operator.                          |
| `bit_op_xor_and_swap`       | Swap bitwise XOR for bitwise AND and vice versa.                       |
| `bool_expr_negate`          | Negate boolean expression.                                             |
| `call_delete`               | Delete call and replace it with `Default::default()`.                  |
| `call_value_default_shadow` | Ignore return value of call by shadowing it with `Default::default()`. |
| `continue_break_swap`       | Swap continue for break and vice versa.                                |
| `eq_op_invert`              | Invert equality check.                                                 |
| `logical_op_and_or_swap`    | Swap logical *and* for logical *or* and vice versa.                    |
| `math_op_add_mul_swap`      | Swap addition for multiplication and vice versa.                       |
| `math_op_add_sub_swap`      | Swap addition for subtraction and vice versa.                          |
| `math_op_div_rem_swap`      | Swap division for modulus and vice versa.                              |
| `math_op_mul_div_swap`      | Swap multiplication for division and vice versa.                       |
| `range_limit_swap`          | Swap limit (inclusivity) of range expression.                          |
| `relational_op_eq_swap`     | Include or remove the boundary (equality) of relational operator.      |
| `relational_op_invert`      | Invert relation operator.                                              |

For more information, and examples, see [docs/operators.md](docs/operators.md).

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
> Currently, the Cargo test target may have to be specified explicitly with the `--lib` or `--bin <BIN>` options, alongside the `-p <PACKAGE>` option. For more targeting options, see `--help`.
>
> Thus, a typical invocation with a library target will look as follows:
>
> ```sh
> cargo mutest --lib run
> ```

Run the `cargo mutest` subcommand against a standard Cargo package directory or workspace containing your crate.

```sh
cargo mutest -p <PACKAGE> run
```

See `--help` for more options and subcommands.

### Using `cfg(mutest)`

When running `cargo mutest`, the `mutest` cfg is set. This can be used to detect if code is running under mutest-rs, and enable conditional compilation based on it.

Starting with Rust 1.80, cfgs are checked against a known set of config names and values. If your Cargo package is checked with a regular Cargo command, it will warn you about the "unexpected" `mutest` cfg. To [let rustc know that this custom cfg is expected](https://blog.rust-lang.org/2024/05/06/check-cfg.html#expecting-custom-cfgs), ensure that `cfg(mutest)` is present in the `[lints.rust.unexpected_cfgs.check-cfg]` array in the package's `Cargo.toml`, like so:

```toml
[lints.rust]
unexpected_cfgs = { level = "warn", check-cfg = ["cfg(mutest)"] }
```

### Annotating code with tool attributes

mutest-rs provides [tool attributes](https://doc.rust-lang.org/reference/attributes.html#tool-attributes) that can be used to optionally annotate your code for use with the tool. Note, that these attributes are only available when running `cargo mutest`, so they need to be wrapped in `#[cfg_attr(mutest, <MUTEST_ATTRIBUTE>)]` for regular Cargo commands to run.

#### `#[mutest::skip]` (use `#[cfg_attr(mutest, mutest::skip)]`)

Tells mutest-rs to skip the function when applying mutation operators. Useful for marking helper functions for tests (test cases themselves are automatically skipped).

This attribute can only be applied to function declarations:
```rs
#[cfg_attr(mutest, mutest::skip)]
fn perform_tests() {
```

#### `#[mutest::ignore]` (use `#[cfg_attr(mutest, mutest::ignore)]`)

Tells mutest-rs to ignore the statement or expression, including any subexpressions, or function parameter, when applying mutation operators. Useful if mutest-rs is trying to apply mutations to a critical piece of code that might be causing problems.

This attribute can be applied to
* statements (note that expression statements might have to be wrapped in `{}`, [see this linked Rust issue](https://github.com/rust-lang/rust/issues/59144)):
  ```rs
  #[cfg_attr(mutest, mutest::ignore)]
  let buff_len = mem::size_of::<u16>() * 1024;
  ```
* expressions ([wherever the compiler supports attrbiutes on expressions](https://doc.rust-lang.org/reference/expressions.html#expression-attributes)):
  ```rs
      #[cfg_attr(mutest, mutest::ignore)]
      Some(body)
  }
   ```
* and function parameters:
  ```rs
  fn foo(&self, #[cfg_attr(mutest, mutest::ignore)] experimental: bool) {
  ```

## License

The mutest-rs project is dual-licensed under Apache 2.0 and MIT terms.

See [LICENSE-APACHE](LICENSE-APACHE), [LICENSE-MIT](LICENSE-MIT), and [COPYRIGHT](COPYRIGHT) for details.
