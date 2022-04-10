#![feature(box_patterns)]
#![feature(decl_macro)]
#![feature(is_some_with)]
#![feature(iter_intersperse)]
#![feature(let_chains)]
#![feature(let_else)]
#![feature(once_cell)]
#![feature(map_try_insert)]

#![feature(rustc_private)]
extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_ast_lowering;
extern crate rustc_ast_pretty;
extern crate rustc_builtin_macros;
extern crate rustc_data_structures;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_resolve;
extern crate rustc_span;

extern crate itertools;
extern crate lazy_static;
extern crate smallvec;

// TODO: Cleanup
extern crate rustc_errors;
extern crate rustc_expand;
extern crate rustc_feature;
extern crate rustc_session;
extern crate rustc_target;
extern crate tracing;

pub mod analysis;
pub mod codegen;
pub mod experiments;
pub mod metadata;

pub use codegen::mutation::Operator;

mod context;
pub use context::*;
