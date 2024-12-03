use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast::{self, P};
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::smallvec::smallvec;

pub const BOOL_EXPR_NEGATE: &str = "bool_expr_negate";

pub struct BoolExprNegateMutation {
    pub was_negated: bool,
}

impl Mutation for BoolExprNegateMutation {
    fn op_name(&self) -> &str { BOOL_EXPR_NEGATE }

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

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts: _, tcx, crate_res: _, def_res: _, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _f) = location else { return Mutations::none(); };

        if let ast::ExprKind::Let(_, _, _, _) = expr.kind { return Mutations::none(); };

        let Some(body_hir) = f_hir.body else { return Mutations::none(); };
        let typeck = tcx.typeck_body(body_hir.id());

        let Some(expr_hir) = body_res.hir_expr(expr) else { unreachable!() };
        let expr_ty = typeck.expr_ty(expr_hir);
        if expr_ty != tcx.types.bool { return Mutations::none(); }

        let negated_expr = ast::mk::expr_unary(def, ast::UnOp::Not, P(expr.clone()));

        let mutation = Self::Mutation {
            was_negated: matches!(&expr.kind, ast::ExprKind::Unary(ast::UnOp::Not, _)),
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id),
                Subst::AstExpr(negated_expr.into_inner()),
            ),
        ])
    }
}
