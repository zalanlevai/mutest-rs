#![feature(decl_macro)]

#![feature(rustc_private)]

mod arg_default_shadow;
pub use arg_default_shadow::*;

mod bool_expr_negate;
pub use bool_expr_negate::*;

mod call_ignore;
pub use call_ignore::*;

mod continue_break_swap;
pub use continue_break_swap::*;

mod eq_op_invert;
pub use eq_op_invert::*;

mod op_swap;
pub use op_swap::*;

mod range_limit_swap;
pub use range_limit_swap::*;

mod relational_op_eq_swap;
pub use relational_op_eq_swap::*;

mod relational_op_invert;
pub use relational_op_invert::*;

pub const ALL: &[&str] = &[
    ARG_DEFAULT_SHADOW,
    BIT_OP_OR_AND_SWAP,
    BIT_OP_OR_XOR_SWAP,
    BIT_OP_SHIFT_DIR_SWAP,
    BIT_OP_XOR_AND_SWAP,
    BOOL_EXPR_NEGATE,
    CALL_DELETE,
    CALL_VALUE_DEFAULT_SHADOW,
    CONTINUE_BREAK_SWAP,
    EQ_OP_INVERT,
    LOGICAL_OP_AND_OR_SWAP,
    MATH_OP_ADD_MUL_SWAP,
    MATH_OP_ADD_SUB_SWAP,
    MATH_OP_DIV_REM_SWAP,
    MATH_OP_MUL_DIV_SWAP,
    RANGE_LIMIT_SWAP,
    RELATIONAL_OP_EQ_SWAP,
    RELATIONAL_OP_INVERT,
];
