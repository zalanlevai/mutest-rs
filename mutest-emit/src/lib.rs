#![feature(decl_macro)]
#![feature(is_some_with)]
#![feature(let_chains)]
#![feature(let_else)]

#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_expand;
extern crate rustc_hash;
extern crate rustc_resolve;
extern crate rustc_session;
extern crate rustc_span;

extern crate itertools;
extern crate lazy_static;
extern crate smallvec;

pub mod analysis;
pub mod codegen;

pub use codegen::mutation::{Mutation, Operator};
