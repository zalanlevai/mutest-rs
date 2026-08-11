#![feature(associated_type_defaults)]
#![feature(decl_macro)]
#![feature(f16)]
#![feature(iter_collect_into)]
#![feature(iter_intersperse)]
#![feature(iterator_try_collect)]
#![feature(macro_metavar_expr)]
#![feature(never_type)]
#![feature(smart_pointer_try_map)]

#![feature(rustc_private)]
extern crate rustc_abi;
extern crate rustc_apfloat;
extern crate rustc_ast;
extern crate rustc_ast_lowering;
extern crate rustc_ast_pretty;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_error_messages;
extern crate rustc_errors;
extern crate rustc_expand;
extern crate rustc_hir;
extern crate rustc_hir_analysis;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_resolve;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;

extern crate itertools;

pub mod analysis;
pub mod codegen;
pub mod session;

pub use codegen::mutation::{Mutation, Operator};
