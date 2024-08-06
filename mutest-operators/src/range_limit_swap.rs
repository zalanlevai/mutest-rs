use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::ty;
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::smallvec::smallvec;

pub const RANGE_LIMIT_SWAP: &str = "range_limit_swap";

pub struct RangeLimitSwapMutation {
    pub replacement_limits: ast::RangeLimits,
}

impl Mutation for RangeLimitSwapMutation {
    fn op_name(&self) -> &str { RANGE_LIMIT_SWAP }

    fn display_name(&self) -> String {
        format!("{operation} range expression",
            operation = match self.replacement_limits {
                ast::RangeLimits::Closed => "include limit in",
                ast::RangeLimits::HalfOpen => "exclude limit from",
            },
        )
    }
}

/// Invert the limits (inclusivity) of range expressions.
pub struct RangeLimitSwap;

impl<'a> Operator<'a> for RangeLimitSwap {
    type Mutation = RangeLimitSwapMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts: _, tcx, def_res: _, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _f) = location else { return Mutations::none(); };

        let ast::ExprKind::Range(start, end, limits) = &expr.kind else { return Mutations::none(); };

        let swapped_limits = match (start, end, limits) {
            | (None, Some(_), ast::RangeLimits::HalfOpen)
            | (Some(_), Some(_), ast::RangeLimits::HalfOpen)
            => ast::RangeLimits::Closed,

            | (None, Some(_), ast::RangeLimits::Closed)
            | (Some(_), Some(_), ast::RangeLimits::Closed)
            => ast::RangeLimits::HalfOpen,

            | (None, None, ast::RangeLimits::HalfOpen)
            | (Some(_), None, ast::RangeLimits::HalfOpen)
            => { return Mutations::none(); }

            _ => unreachable!(),
        };

        // HACK: Simply changing the limits on the syntax level does not work, because the desugared
        //       types are different for each kind of range expression.
        //
        //       Instead, we increment/decrement the end bound of the range to emulate the same
        //       behaviour for the common cases (integers), where this is possible.
        //
        //       Eventually, we will be able to do this instead:
        //       ```
        //       ast::mk::expr_range(def, start.clone(), end.clone(), swapped_limits);
        //       ```
        let swapped_limits_range_expr = {
            let Some(body_hir) = f_hir.body else { return Mutations::none(); };
            let typeck = tcx.typeck_body(body_hir.id());

            let Some(expr_hir) = body_res.hir_expr(expr) else { unreachable!() };
            let range_ty = typeck.node_type(expr_hir.hir_id);
            let ty::TyKind::Adt(_, range_ty_generics) = range_ty.kind() else { unreachable!() };
            let range_bound_ty = range_ty_generics.type_at(0);
            if !range_bound_ty.is_integral() { return Mutations::none(); }

            let Some(end) = end else { unreachable!(); };

            match swapped_limits {
                ast::RangeLimits::Closed => {
                    let incremented_end = ast::mk::expr_paren(def, ast::mk::expr_binary(def, ast::BinOpKind::Add, end.clone(), ast::mk::expr_int(def, 1)));
                    ast::mk::expr_range(def, start.clone(), Some(incremented_end), ast::RangeLimits::HalfOpen)
                }
                ast::RangeLimits::HalfOpen => {
                    let decremented_end = ast::mk::expr_paren(def, ast::mk::expr_binary(def, ast::BinOpKind::Sub, end.clone(), ast::mk::expr_int(def, 1)));
                    ast::mk::expr_range(def, start.clone(), Some(decremented_end), ast::RangeLimits::Closed)
                }
            }
        };

        let mutation = Self::Mutation {
            replacement_limits: swapped_limits,
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id),
                Subst::AstExpr(swapped_limits_range_expr.into_inner()),
            ),
        ])
    }
}
