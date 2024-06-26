use mutest_emit::{Mutation, Operator};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use mutest_emit::smallvec::{SmallVec, smallvec};

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

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
        let MutCtxt { opts: _, tcx: _, def_res: _, def_site: def, item_hir: _, body_res: _, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _) = location else { return None; };

        let swapped_expr = match &expr.kind {
            ast::ExprKind::Continue(label) => {
                ast::mk::expr(def, ast::ExprKind::Break(*label, None))
            }
            ast::ExprKind::Break(label, None) => {
                ast::mk::expr(def, ast::ExprKind::Continue(*label))
            }
            _ => { return None; }
        };

        let mutation = Self::Mutation {
            original_expr: expr.kind.clone(),
            replacement_expr: swapped_expr.kind.clone(),
        };

        Some((mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id),
                Subst::AstExpr(swapped_expr.into_inner()),
            ),
        ]))
    }
}
