use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use mutest_emit::smallvec::{SmallVec, smallvec};

macro define_logical_op_swap_mutation(
    $(#[$meta:meta])*
    $vis:vis $operator:ident, $mutation:ident {
        $($bin_op_from:pat => $bin_op_to:expr),+ $(,)?
    }
) {
    $vis struct $mutation {
        pub is_assignment: bool,
        pub original_bin_op: ast::BinOpKind,
        pub replacement_bin_op: ast::BinOpKind,
    }

    impl Mutation for $mutation {
        fn display_name(&self) -> String {
            format!("swap logical {expr_kind} `{original_bin_op}` for `{replacement_bin_op}`",
                expr_kind = match self.is_assignment {
                    true => "assignment operator",
                    false => "operator",
                },
                original_bin_op = self.original_bin_op.to_string(),
                replacement_bin_op = self.replacement_bin_op.to_string(),
            )
        }

        fn span_label(&self) -> String {
            format!("swap logical {expr_kind} for `{replacement_bin_op}`",
                expr_kind = match self.is_assignment {
                    true => "assignment operator",
                    false => "operator",
                },
                replacement_bin_op = self.replacement_bin_op.to_string(),
            )
        }
    }

    $(#[$meta])*
    $vis struct $operator;

    impl<'a> Operator<'a> for $operator {
        type Mutation = $mutation;

        fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
            let MutCtxt { tcx: _, resolver: _, def_site: def, ref location } = *mcx;

            let MutLoc::FnBodyExpr(expr, _) = location else { return None; };

            let bin_op = match &expr.ast.kind {
                ast::ExprKind::Binary(bin_op, _, _) => bin_op,
                ast::ExprKind::AssignOp(bin_op, _, _) => bin_op,
                _ => { return None; }
            };

            let mapped_bin_op = match bin_op.node {
                $($bin_op_from => $bin_op_to,)+
                _ => { return None; },
            };

            let mapped_bin_expr = match &expr.ast.kind {
                ast::ExprKind::Binary(_, lhs, rhs) => ast::mk::expr_binary(def, mapped_bin_op, lhs.clone(), rhs.clone()),
                ast::ExprKind::AssignOp(_, lhs, rhs) => ast::mk::expr_assign_op(def, mapped_bin_op, lhs.clone(), rhs.clone()),
                _ => unreachable!(),
            };

            let mutation = Self::Mutation {
                is_assignment: matches!(&expr.ast.kind, ast::ExprKind::AssignOp(_, _, _)),
                original_bin_op: bin_op.node,
                replacement_bin_op: mapped_bin_op,
            };

            Some((mutation, smallvec![
                SubstDef::new(
                    SubstLoc::Replace(expr.ast.id),
                    Subst::AstExpr(mapped_bin_expr.into_inner()),
                ),
            ]))
        }
    }
}

define_logical_op_swap_mutation! {
    /// Swap logical && for logical || and vice versa.
    pub LogicalOpAndOrSwap, LogicalOpAndOrSwapMutation {
        ast::BinOpKind::And => ast::BinOpKind::Or,
        ast::BinOpKind::Or => ast::BinOpKind::And,
    }
}
