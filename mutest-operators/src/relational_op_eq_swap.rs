use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use mutest_emit::smallvec::{SmallVec, smallvec};

pub struct RelationalOpEqSwapMutation {
    pub original_bin_op: ast::BinOpKind,
    pub replacement_bin_op: ast::BinOpKind,
}

impl Mutation for RelationalOpEqSwapMutation {
    fn display_name(&self) -> String {
        format!("{operation} relational operator `{original_bin_op}`",
            operation = match self.replacement_bin_op {
                ast::BinOpKind::Le | ast::BinOpKind::Ge => "add equality to",
                ast::BinOpKind::Lt | ast::BinOpKind::Gt => "remove equality from",
                _ => unreachable!(),
            },
            original_bin_op = self.original_bin_op.to_string(),
        )
    }
}

/// Include or remove the boundary (equality) of relational operators.
pub struct RelationalOpEqSwap;

impl<'a> Operator<'a> for RelationalOpEqSwap {
    type Mutation = RelationalOpEqSwapMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
        let MutCtxt { tcx: _, resolver: _, def_site: def, ref location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _) = location else { return None; };

        let ast::ExprKind::Binary(bin_op, lhs, rhs) = &expr.ast.kind else { return None; };

        let inverted_eq_bin_op = match bin_op.node {
            ast::BinOpKind::Lt => ast::BinOpKind::Le,
            ast::BinOpKind::Le => ast::BinOpKind::Lt,
            ast::BinOpKind::Gt => ast::BinOpKind::Ge,
            ast::BinOpKind::Ge => ast::BinOpKind::Gt,
            _ => { return None; },
        };
        let inverted_eq_bin_expr = ast::mk::expr_binary(def, inverted_eq_bin_op, lhs.clone(), rhs.clone());

        let mutation = Self::Mutation {
            original_bin_op: bin_op.node,
            replacement_bin_op: inverted_eq_bin_op,
        };

        Some((mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.ast.id),
                Subst::AstExpr(inverted_eq_bin_expr.into_inner()),
            ),
        ]))
    }
}
