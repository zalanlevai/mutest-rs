#![feature(let_else)]

#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_ast_lowering;
extern crate rustc_span;
extern crate smallvec;

mod replace_arg_with_default;
pub use replace_arg_with_default::*;
