use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::hir;
use mutest_emit::analysis::res;
use mutest_emit::analysis::ty::{self, Ty, TyCtxt};
use mutest_emit::codegen::ast::{self, P};
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use mutest_emit::codegen::symbols::{Ident, path, kw};
use mutest_emit::smallvec::{SmallVec, smallvec};

fn non_default_call<'tcx>(tcx: TyCtxt<'tcx>, body: hir::BodyId, expr: &'tcx hir::Expr<'tcx>, limit_scope_to_local_callees: bool) -> Option<(hir::DefId, Ty<'tcx>)> {
    // Calls to functions that take no arguments (including self) are ignored, because they are likely
    // default constructor functions.
    let call_args_count = match expr.kind {
        hir::ExprKind::Call(_, args) => args.len(),
        hir::ExprKind::MethodCall(_, args, _) => args.len(),
        _ => unreachable!(),
    };
    if call_args_count == 0 { return None; }

    let typeck = tcx.typeck_body(body);

    let expr_ty = typeck.expr_ty(expr);
    if expr_ty == tcx.types.unit || expr_ty == tcx.types.never { return None; }
    if !ty::impls_trait(tcx, expr_ty, res::traits::Default(tcx)) { return None; }

    let Some((callee, _)) = res::callee(typeck, expr) else { return None; };
    if limit_scope_to_local_callees && !callee.is_local() { return None; }
    if callee == res::fns::default(tcx) { return None; }

    return Some((callee, expr_ty))
}

pub struct CallValueDefaultShadowMutation {
    pub callee_path: String,
}

impl Mutation for CallValueDefaultShadowMutation {
    fn display_name(&self) -> String {
        format!("ignore return value of call to `{callee}` by shadowing it with `Default::default()`",
            callee = self.callee_path
        )
    }

    fn span_label(&self) -> String {
        "ignore return value of call by shadowing it with `Default::default()`".to_owned()
    }
}

/// Replace the return value of function calls with `Default::default()` to test whether the return
/// values of inner calls are meaningfully tested, while retaining expected side-effects of the
/// callees.
pub struct CallValueDefaultShadow {
    pub limit_scope_to_local_callees: bool,
}

impl<'a> Operator<'a> for CallValueDefaultShadow {
    type Mutation = CallValueDefaultShadowMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
        let MutCtxt { tcx, resolver: _, def_site: def, ref location } = *mcx;

        let MutLoc::FnBodyExpr(expr, f) = location else { return None; };

        let (ast::ExprKind::Call(..) | ast::ExprKind::MethodCall(..)) = expr.ast.kind else { return None; };

        let Some((callee, expr_ty)) = non_default_call(
            tcx,
            f.hir.body.id(),
            expr.hir,
            self.limit_scope_to_local_callees,
        ) else { return None; };

        // A type annotation with the originally resolved type has to be added to the ignoring
        // `let _ = $expr` statement to guarantee the same callee resolution.
        let Some(expr_ty_ast) = ty::ast_repr(tcx, def, expr_ty) else { return None; };

        // Default::default()
        let default = ast::mk::expr_call_path(def, path::default(def), vec![]);
        // { let _: $ty = $expr; Default::default() }
        let shadow_scope = ast::mk::expr_block(ast::mk::block(def, vec![
            ast::mk::stmt_let(def, false, Ident::new(kw::Underscore, def), Some(expr_ty_ast), P(expr.ast.clone())),
            ast::mk::stmt_expr(default),
        ]));

        let mutation = Self::Mutation {
            callee_path: tcx.def_path_str(callee),
        };

        Some((mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.ast.id),
                Subst::AstExpr(shadow_scope.into_inner()),
            ),
        ]))
    }
}

pub struct CallDeleteMutation {
    pub callee_path: String,
}

impl Mutation for CallDeleteMutation {
    fn display_name(&self) -> String {
        format!("delete call to `{callee}` and replace it with `Default::default()`",
            callee = self.callee_path
        )
    }

    fn span_label(&self) -> String {
        "delete call and replace it with `Default::default()`".to_owned()
    }
}

/// Delete function calls and replace them with `Default::default()` to test whether inner calls are
/// meaningfully tested, without retaining any side-effects of the callees.
pub struct CallDelete {
    pub limit_scope_to_local_callees: bool,
}

impl<'a> Operator<'a> for CallDelete {
    type Mutation = CallDeleteMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
        let MutCtxt { tcx, resolver: _, def_site: def, ref location } = *mcx;

        let MutLoc::FnBodyExpr(expr, f) = location else { return None; };

        let (ast::ExprKind::Call(..) | ast::ExprKind::MethodCall(..)) = expr.ast.kind else { return None; };

        let Some((callee, _)) = non_default_call(
            tcx,
            f.hir.body.id(),
            expr.hir,
            self.limit_scope_to_local_callees,
        ) else { return None; };

        // Default::default()
        let default = ast::mk::expr_call_path(def, path::default(def), vec![]);

        let mutation = Self::Mutation {
            callee_path: tcx.def_path_str(callee),
        };

        Some((mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.ast.id),
                Subst::AstExpr(default.into_inner()),
            ),
        ]))
    }
}
