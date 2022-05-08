use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast::{self, P};
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use smallvec::{SmallVec, smallvec};

pub struct BoolExprNegateMutation {
    pub was_negated: bool,
}

impl Mutation for BoolExprNegateMutation {
    fn display_name(&self) -> String {
        format!("{operation} boolean expression",
            operation = match self.was_negated {
                false => "negate",
                true => "remove negation from",
            },
        )
    }
}

/// Negate boolean expressions.
pub struct BoolExprNegate;

impl<'a> Operator<'a> for BoolExprNegate {
    type Mutation = BoolExprNegateMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
        let MutCtxt { tcx, resolver: _, def_site: def, ref location } = *mcx;

        let MutLoc::FnBodyExpr(expr, f) = location else { return None; };

        let typeck = tcx.typeck_body(f.hir.body.id());

        let expr_ty = typeck.expr_ty(expr.hir);
        if expr_ty != tcx.types.bool { return None; }

        let negated_expr = ast::mk::expr_unary(def, ast::UnOp::Not, P(expr.ast.clone()));

        let mutation = Self::Mutation {
            was_negated: matches!(&expr.ast.kind, ast::ExprKind::Unary(ast::UnOp::Not, _)),
        };

        Some((mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.ast.id),
                Subst::AstExpr(negated_expr.into_inner()),
            ),
        ]))
    }
}
