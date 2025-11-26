use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::smallvec::smallvec;

pub const RELATIONAL_OP_INVERT: &str = "relational_op_invert";

pub struct RelationalOpInvertMutation {
    pub original_bin_op: ast::BinOpKind,
    pub replacement_bin_op: ast::BinOpKind,
}

impl Mutation for RelationalOpInvertMutation {
    fn op_name(&self) -> &str { RELATIONAL_OP_INVERT }

    fn display_name(&self) -> String {
        format!("invert relational operator `{original_bin_op}` for `{replacement_bin_op}`",
            original_bin_op = self.original_bin_op.as_str(),
            replacement_bin_op = self.replacement_bin_op.as_str(),
        )
    }

    fn span_label(&self) -> String {
        format!("invert relational operator for `{replacement_bin_op}`",
            replacement_bin_op = self.replacement_bin_op.as_str(),
        )
    }
}

/// Completely invert relation operators.
pub struct RelationalOpInvert;

impl<'a> Operator<'a> for RelationalOpInvert {
    type Mutation = RelationalOpInvertMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts: _, tcx: _, crate_res: _, def_res: _, def_site: def, item_hir: _, body_res: _, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _) = location else { return Mutations::none(); };

        let ast::ExprKind::Binary(bin_op, lhs, rhs) = &expr.kind else { return Mutations::none(); };

        let inverted_bin_op = match bin_op.node {
            ast::BinOpKind::Lt => ast::BinOpKind::Ge,
            ast::BinOpKind::Le => ast::BinOpKind::Gt,
            ast::BinOpKind::Gt => ast::BinOpKind::Le,
            ast::BinOpKind::Ge => ast::BinOpKind::Lt,
            _ => { return Mutations::none(); },
        };
        let inverted_bin_expr = ast::mk::expr_binary(def, inverted_bin_op, lhs.clone(), rhs.clone());

        let mutation = Self::Mutation {
            original_bin_op: bin_op.node,
            replacement_bin_op: inverted_bin_op,
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id, expr.span),
                Subst::AstExpr(*inverted_bin_expr),
            ),
        ])
    }
}
