#![feature(associated_type_defaults)]
#![feature(decl_macro)]
#![feature(extract_if)]
#![feature(f16)]
#![feature(f128)]
#![feature(if_let_guard)]
#![feature(iter_collect_into)]
#![feature(iter_intersperse)]
#![feature(iterator_try_collect)]
#![feature(lazy_cell)]
#![feature(let_chains)]

#![feature(rustc_private)]
extern crate rustc_apfloat;
extern crate rustc_ast;
extern crate rustc_ast_lowering;
extern crate rustc_ast_pretty;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_error_messages;
extern crate rustc_errors;
extern crate rustc_expand;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_hir_analysis;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_query_system;
extern crate rustc_resolve;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;

extern crate itertools;
extern crate lazy_static;
extern crate termcolor;
// HACK: When compiling mutest-operators, the compiler is unable to resolve the right version of smallvec. We have to
//       use the version used by the compiler to interface with it but we also expose APIs which use smallvec for use by
//       other crates. To resolve this, we re-export the compiler-supplied version of the library.
pub extern crate smallvec;
pub extern crate thin_vec;

pub mod analysis;
pub mod codegen;
pub mod session;

pub use codegen::mutation::{Mutation, Operator};
