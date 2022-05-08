#![feature(decl_macro)]
#![feature(let_else)]

#![feature(rustc_private)]
extern crate smallvec;

mod arg_default_shadow;
pub use arg_default_shadow::*;

mod bit_op_swap;
pub use bit_op_swap::*;

mod call_ignore;
pub use call_ignore::*;

mod continue_break_swap;
pub use continue_break_swap::*;

mod eq_op_invert;
pub use eq_op_invert::*;

mod logical_op_swap;
pub use logical_op_swap::*;

mod math_op_swap;
pub use math_op_swap::*;

mod range_limit_swap;
pub use range_limit_swap::*;

mod relational_op_eq_swap;
pub use relational_op_eq_swap::*;

mod relational_op_invert;
pub use relational_op_invert::*;
