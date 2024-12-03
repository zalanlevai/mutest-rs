use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::hir;
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::smallvec::smallvec;

pub const CONTINUE_BREAK_SWAP: &str = "continue_break_swap";

pub struct ContinueBreakSwapMutation {
    pub original_expr: ast::ExprKind,
    pub replacement_expr: ast::ExprKind,
}

impl Mutation for ContinueBreakSwapMutation {
    fn op_name(&self) -> &str { CONTINUE_BREAK_SWAP }

    fn display_name(&self) -> String {
        let display_expr = |expr: &ast::ExprKind| match expr {
            ast::ExprKind::Break(Some(label), _) => format!("break with label `{}`", label.ident),
            ast::ExprKind::Break(None, _) => "break".to_owned(),
            ast::ExprKind::Continue(Some(label)) => format!("continue with label `{}`", label.ident),
            ast::ExprKind::Continue(None) => "continue".to_owned(),
            _ => unreachable!(),
        };

        format!("swap {original_expr} for {replacement_expr}",
            original_expr = display_expr(&self.original_expr),
            replacement_expr = display_expr(&self.replacement_expr),
        )
    }

    fn span_label(&self) -> String {
        let display_expr = |expr: &ast::ExprKind| match expr {
            ast::ExprKind::Break(Some(label), _) => format!("break with label `{}`", label.ident),
            ast::ExprKind::Break(None, _) => "break".to_owned(),
            ast::ExprKind::Continue(Some(label)) => format!("continue with label `{}`", label.ident),
            ast::ExprKind::Continue(None) => "continue".to_owned(),
            _ => unreachable!(),
        };

        format!("swap for {replacement_expr}",
            replacement_expr = display_expr(&self.replacement_expr),
        )
    }
}

/// Swap continue expressions for break expressions and vice versa.
pub struct ContinueBreakSwap;

impl<'a> Operator<'a> for ContinueBreakSwap {
    type Mutation = ContinueBreakSwapMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts: _, tcx, crate_res: _, def_res: _, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _) = location else { return Mutations::none(); };

        let swapped_expr = match &expr.kind {
            ast::ExprKind::Continue(label) => {
                ast::mk::expr(def, ast::ExprKind::Break(*label, None))
            }
            ast::ExprKind::Break(label, None) => {
                ast::mk::expr(def, ast::ExprKind::Continue(*label))
            }
            _ => { return Mutations::none(); }
        };

        let Some(body_hir) = f_hir.body else { return Mutations::none(); };
        let typeck = tcx.typeck_body(body_hir.id());

        let Some(expr_hir) = body_res.hir_expr(expr) else { unreachable!() };

        let (hir::ExprKind::Continue(destination) | hir::ExprKind::Break(destination, _)) = expr_hir.kind else { unreachable!() };
        let target_hir_id = destination.target_id.unwrap();
        let target_ty = typeck.node_type(target_hir_id);
        if target_ty != tcx.types.unit && target_ty != tcx.types.never { return Mutations::none(); }

        let mutation = Self::Mutation {
            original_expr: expr.kind.clone(),
            replacement_expr: swapped_expr.kind.clone(),
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id),
                Subst::AstExpr(swapped_expr.into_inner()),
            ),
        ])
    }
}
