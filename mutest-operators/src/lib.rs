#![feature(decl_macro)]
#![feature(let_else)]

#![feature(rustc_private)]
extern crate smallvec;

mod arg_default_shadow;
pub use arg_default_shadow::*;

mod eq_op_invert;
pub use eq_op_invert::*;

mod math_op_swap;
pub use math_op_swap::*;

mod relational_op_eq_swap;
pub use relational_op_eq_swap::*;

mod relational_op_invert;
pub use relational_op_invert::*;
