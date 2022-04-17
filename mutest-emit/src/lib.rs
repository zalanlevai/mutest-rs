#![feature(associated_type_defaults)]
#![feature(decl_macro)]
#![feature(is_some_with)]
#![feature(let_chains)]
#![feature(let_else)]
#![feature(once_cell)]

#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_ast_lowering;
extern crate rustc_data_structures;
extern crate rustc_error_messages;
extern crate rustc_errors;
extern crate rustc_expand;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_infer;
extern crate rustc_middle;
extern crate rustc_resolve;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_trait_selection;
extern crate rustc_typeck;

extern crate itertools;
extern crate lazy_static;
extern crate smallvec;

pub mod analysis;
pub mod codegen;

pub use codegen::mutation::{Mutation, Operator};
