use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::call_graph;
use mutest_emit::analysis::hir;
use mutest_emit::analysis::res;
use mutest_emit::analysis::ty::{self, Ty, TyCtxt};
use mutest_emit::codegen::ast::{self, P};
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::codegen::symbols::{Ident, path, kw};
use mutest_emit::thin_vec::thin_vec;
use mutest_emit::smallvec::smallvec;

fn non_default_call<'tcx>(tcx: TyCtxt<'tcx>, f: hir::LocalDefId, body: hir::BodyId, expr: &'tcx hir::Expr<'tcx>, limit_scope_to_local_callees: bool) -> Option<(hir::DefId, Ty<'tcx>)> {
    // Calls to functions that take no arguments (including self) are ignored, because they are likely
    // default constructor functions.
    let call_args_count = match expr.kind {
        hir::ExprKind::Call(_, args) => args.len(),
        hir::ExprKind::MethodCall(_, _, args, _) => 1 + args.len(),
        _ => unreachable!(),
    };
    if call_args_count == 0 { return None; }

    let param_env = tcx.param_env(f);
    let typeck = tcx.typeck_body(body);

    let expr_ty = typeck.expr_ty(expr);
    if expr_ty == tcx.types.unit || expr_ty == tcx.types.never { return None; }
    if !ty::impls_trait(tcx, f, expr_ty, res::traits::Default(tcx), vec![]) { return None; }

    let Some((callee, _)) = res::callee(typeck, expr) else { return None; };
    if limit_scope_to_local_callees && !callee.is_local() { return None; }
    if callee == res::fns::default(tcx) { return None; }

    // Avoid replacing the call with `Default::default`
    // if the expression-containing function is the type's `Default::default` implementation, or
    // if the type's `Default::default` implementation refers back to the expression-containing function
    // (the function this call expression is in).
    // This avoids a case of infinite recursion, resulting in a stack overflow.
    let ty_default = res::fns::default(tcx);
    let ty_default_generic_args = tcx.mk_args_trait(expr_ty, vec![]);
    let typing_env = ty::TypingEnv { typing_mode: ty::TypingMode::PostAnalysis, param_env };
    if let Some(ty_default_impl) = ty::Instance::try_resolve(tcx, typing_env, ty_default, ty_default_generic_args).ok().flatten()
        && let Some(ty_default_impl_def_id) = ty_default_impl.def_id().as_local()
        && let Some(ty_default_impl_body_id) = tcx.hir_node_by_def_id(ty_default_impl_def_id).body_id()
    {
        // Direct recursion: placing a call to type `T`'s `Default::default` implementation
        // within type `T`'s `Default::default` implementation is a directly recursive function call.
        if ty_default_impl_def_id == f { return None; }

        let mut ty_default_impl_callees = res::collect_callees(tcx, tcx.hir_body(ty_default_impl_body_id)).into_iter()
            .filter_map(|call| match call.kind {
                call_graph::CallKind::Def(def_id, generic_args) => Some((def_id, generic_args)),
                _ => None,
            })
            .flat_map(|(def_id, generic_args)| {
                let param_env = tcx.param_env(def_id);
                let typing_env = ty::TypingEnv { typing_mode: ty::TypingMode::PostAnalysis, param_env };
                ty::Instance::try_resolve(tcx, typing_env, def_id, generic_args).ok().flatten()
            });

        let ty_default_impl_refers_to_call = ty_default_impl_callees.any(|instance| instance.def_id() == f.to_def_id());
        if ty_default_impl_refers_to_call { return None; }
    }

    Some((callee, expr_ty))
}

pub const CALL_VALUE_DEFAULT_SHADOW: &str = "call_value_default_shadow";

pub struct CallValueDefaultShadowMutation {
    pub callee_path: String,
}

impl Mutation for CallValueDefaultShadowMutation {
    fn op_name(&self) -> &str { CALL_VALUE_DEFAULT_SHADOW }

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

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts, tcx, crate_res, def_res, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _f) = location else { return Mutations::none(); };
        let Some(body_hir) = f_hir.body else { return Mutations::none(); };

        let (ast::ExprKind::Call(..) | ast::ExprKind::MethodCall(..)) = expr.kind else { return Mutations::none(); };

        let Some(expr_hir) = body_res.hir_expr(expr) else { unreachable!() };
        let Some((callee, expr_ty)) = non_default_call(
            tcx,
            f_hir.owner_id.def_id,
            body_hir.id(),
            expr_hir,
            self.limit_scope_to_local_callees,
        ) else { return Mutations::none(); };

        // A type annotation with the originally resolved type has to be added to the ignoring
        // `let _ = $expr` statement to guarantee the same callee resolution.
        let scope = f_hir.owner_id.def_id.to_def_id();
        let def_path_handling = ty::print::DefPathHandling::PreferVisible(ty::print::ScopedItemPaths::Trimmed);
        let opaque_ty_handling = ty::print::OpaqueTyHandling::Infer;
        let Some(expr_ty_ast) = ty::ast_repr(tcx, crate_res, def_res, Some(scope), def, expr_ty, def_path_handling, opaque_ty_handling, opts.sanitize_macro_expns, f_hir.owner_id.to_def_id()) else { return Mutations::none(); };

        // Default::default()
        let default = ast::mk::expr_call_path(def, path::default(def), thin_vec![]);
        // { let _: $ty = $expr; Default::default() }
        let shadow_scope = ast::mk::expr_block(ast::mk::block(def, thin_vec![
            ast::mk::stmt_let(def, false, Ident::new(kw::Underscore, def), Some(expr_ty_ast), P(expr.clone())),
            ast::mk::stmt_expr(default),
        ]));

        let mutation = Self::Mutation {
            callee_path: tcx.def_path_str(callee),
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id),
                Subst::AstExpr(shadow_scope.into_inner()),
            ),
        ])
    }
}

pub const CALL_DELETE: &str = "call_delete";

pub struct CallDeleteMutation {
    pub callee_path: String,
}

impl Mutation for CallDeleteMutation {
    fn op_name(&self) -> &str { CALL_DELETE }

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

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts: _, tcx, crate_res: _, def_res: _, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

        let MutLoc::FnBodyExpr(expr, _f) = location else { return Mutations::none(); };
        let Some(body_hir) = f_hir.body else { return Mutations::none(); };

        let (ast::ExprKind::Call(..) | ast::ExprKind::MethodCall(..)) = expr.kind else { return Mutations::none(); };

        let Some(expr_hir) = body_res.hir_expr(expr) else { unreachable!() };
        let Some((callee, _)) = non_default_call(
            tcx,
            f_hir.owner_id.def_id,
            body_hir.id(),
            expr_hir,
            self.limit_scope_to_local_callees,
        ) else { return Mutations::none(); };

        // Default::default()
        let default = ast::mk::expr_call_path(def, path::default(def), thin_vec![]);

        let mutation = Self::Mutation {
            callee_path: tcx.def_path_str(callee),
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id),
                Subst::AstExpr(default.into_inner()),
            ),
        ])
    }
}
