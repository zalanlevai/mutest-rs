use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::smallvec::smallvec;

pub const RELATIONAL_OP_EQ_SWAP: &str = "relational_op_eq_swap";

pub struct RelationalOpEqSwapMutation {
    pub original_bin_op: ast::BinOpKind,
    pub replacement_bin_op: ast::BinOpKind,
}

impl Mutation for RelationalOpEqSwapMutation {
    fn op_name(&self) -> &str { RELATIONAL_OP_EQ_SWAP }

    fn display_name(&self) -> String {
        format!("{operation} relational operator `{original_bin_op}`",
            operation = match self.replacement_bin_op {
                ast::BinOpKind::Le | ast::BinOpKind::Ge => "add equality to",
                ast::BinOpKind::Lt | ast::BinOpKind::Gt => "remove equality from",
                _ => unreachable!(),
            },
            original_bin_op = self.original_bin_op.as_str(),
        )
    }

    fn span_label(&self) -> String {
        format!("{operation} relational operator",
            operation = match self.replacement_bin_op {
                ast::BinOpKind::Le | ast::BinOpKind::Ge => "add equality to",
                ast::BinOpKind::Lt | ast::BinOpKind::Gt => "remove equality from",
                _ => unreachable!(),
            },
        )
    }
}

/// Include or remove the boundary (equality) of relational operators.
pub struct RelationalOpEqSwap;

impl<'a> Operator<'a> for RelationalOpEqSwap {
    type Mutation = RelationalOpEqSwapMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts: _, tcx: _, crate_res: _, def_res: _, def_site: def, item_hir: _, body_res: _, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _) = location else { return Mutations::none(); };

        let ast::ExprKind::Binary(bin_op, lhs, rhs) = &expr.kind else { return Mutations::none(); };

        let inverted_eq_bin_op = match bin_op.node {
            ast::BinOpKind::Lt => ast::BinOpKind::Le,
            ast::BinOpKind::Le => ast::BinOpKind::Lt,
            ast::BinOpKind::Gt => ast::BinOpKind::Ge,
            ast::BinOpKind::Ge => ast::BinOpKind::Gt,
            _ => { return Mutations::none(); },
        };
        let inverted_eq_bin_expr = ast::mk::expr_binary(def, inverted_eq_bin_op, lhs.clone(), rhs.clone());

        let mutation = Self::Mutation {
            original_bin_op: bin_op.node,
            replacement_bin_op: inverted_eq_bin_op,
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id, expr.span),
                Subst::AstExpr(*inverted_eq_bin_expr),
            ),
        ])
    }
}
