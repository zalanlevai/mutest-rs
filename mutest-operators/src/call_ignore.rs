use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::call_graph;
use mutest_emit::analysis::hir;
use mutest_emit::analysis::res;
use mutest_emit::analysis::ty::{self, Ty};
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::codegen::symbols::{Ident, path, kw};
use rustc_data_structures::smallvec::smallvec;
use rustc_data_structures::thin_vec::thin_vec;
use rustc_middle::ty::{TyCtxt, TypeVisitableExt};

/// Whether the place (or value) produced by the expression is rooted in a temporary,
/// i.e. it is not reached through a local, a static, or a pointer dereference.
fn is_rooted_in_temporary<'tcx>(typeck: &'tcx ty::TypeckResults<'tcx>, expr: &'tcx hir::Expr<'tcx>) -> bool {
    match expr.kind {
        hir::ExprKind::Lit(_) => false,
        // NOTE: Only locals and statics are places. Paths to constants, const params, and unit constructors produce new values.
        hir::ExprKind::Path(ref qpath) => !matches!(typeck.qpath_res(qpath, expr.hir_id), hir::Res::Local(_) | hir::Res::Def(hir::DefKind::Static { .. }, _)),
        hir::ExprKind::Unary(hir::UnOp::Deref, base) if typeck.expr_ty_adjusted(base).is_any_ptr() => false,
        hir::ExprKind::Unary(hir::UnOp::Deref, base) | hir::ExprKind::Field(base, _) | hir::ExprKind::Index(base, _, _) => {
            adjusted_place_is_rooted_in_temporary(typeck, base)
        }
        _ => !expr.is_place_expr(|_| true),
    }
}

/// Like `is_rooted_in_temporary`, but for the place the expression is adjusted to,
/// e.g. by autoderef of a field access or method receiver.
fn adjusted_place_is_rooted_in_temporary<'tcx>(typeck: &'tcx ty::TypeckResults<'tcx>, expr: &'tcx hir::Expr<'tcx>) -> bool {
    let mut source_ty = typeck.expr_ty(expr);
    for adjustment in typeck.expr_adjustments(expr) {
        // Places behind a pointer are not owned by the temporary pointer itself.
        if let ty::adjustment::Adjust::Deref(ty::adjustment::DerefAdjustKind::Builtin) = adjustment.kind && source_ty.is_any_ptr() { return false; }
        if let ty::adjustment::Adjust::Deref(ty::adjustment::DerefAdjustKind::Overloaded(_)) = adjustment.kind { break; }
        source_ty = adjustment.target;
    }
    is_rooted_in_temporary(typeck, expr)
}

/// Whether evaluating the expression borrows a temporary it creates,
/// e.g. an autoref'd receiver of a method call, or a field of a temporary.
fn borrows_temporaries<'tcx>(typeck: &'tcx ty::TypeckResults<'tcx>, expr: &'tcx hir::Expr<'tcx>) -> bool {
    struct TemporaryBorrowFinder<'tcx> {
        typeck: &'tcx ty::TypeckResults<'tcx>,
        found: bool,
    }

    impl<'tcx> hir::intravisit::Visitor<'tcx> for TemporaryBorrowFinder<'tcx> {
        fn visit_expr(&mut self, expr: &'tcx hir::Expr<'tcx>) {
            if self.found { return; }

            let explicitly_borrows_temporary = matches!(expr.kind, hir::ExprKind::AddrOf(_, _, inner) if is_rooted_in_temporary(self.typeck, inner));
            let adjustments_borrow_temporary = self.typeck.expr_adjustments(expr).iter().any(|adjustment| matches!(adjustment.kind,
                | ty::adjustment::Adjust::Borrow(_)
                | ty::adjustment::Adjust::Deref(ty::adjustment::DerefAdjustKind::Overloaded(_))
            )) && adjusted_place_is_rooted_in_temporary(self.typeck, expr);
            if explicitly_borrows_temporary || adjustments_borrow_temporary {
                self.found = true;
                return;
            }

            // NOTE: Closure bodies are evaluated separately.
            if let hir::ExprKind::Closure(_) = expr.kind { return; }

            hir::intravisit::walk_expr(self, expr);
        }
    }

    let mut finder = TemporaryBorrowFinder { typeck, found: false };
    hir::intravisit::walk_expr(&mut finder, expr);
    finder.found
}

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
    // The replacement `Default::default()` is inferred to the type the expression is coerced to (e.g. `Box<dyn Trait>`).
    let expr_ty_adjusted = typeck.expr_ty_adjusted(expr);
    if expr_ty_adjusted != expr_ty && !ty::impls_trait(tcx, f, expr_ty_adjusted, res::traits::Default(tcx), vec![]) { return None; }

    // The original call is moved into an arm of the substitution `match`, which drops the temporaries of the call
    // at the end of the arm, so a returned borrow of such a temporary would not live long enough.
    if (expr_ty.has_erased_regions() || expr_ty.has_free_regions()) && borrows_temporaries(typeck, expr) { return None; }

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
    let typing_env = ty::TypingEnv::new(param_env, ty::TypingMode::PostAnalysis);
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
                let typing_env = ty::TypingEnv::new(param_env, ty::TypingMode::PostAnalysis);
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
        let MutCtxt { opts: _, tcx, crate_res, def_res, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

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
        let opaque_ty_handling = ty::print::OpaqueTyHandling::Infer;
        let Some(expr_ty_ast) = ty::print::ty_ast(tcx, crate_res, def_res, Some(scope), def, expr_ty, opaque_ty_handling, f_hir.owner_id.to_def_id()) else { return Mutations::none(); };

        // Default::default()
        let default = ast::mk::expr_call_path(def, path::default(def), thin_vec![]);
        // { let _: $ty = $expr; Default::default() }
        let shadow_scope = ast::mk::expr_block(ast::mk::block(def, thin_vec![
            ast::mk::stmt_let(def, false, Ident::new(kw::Underscore, def), Some(expr_ty_ast), Box::new(expr.clone())),
            ast::mk::stmt_expr(default),
        ]));

        let mutation = Self::Mutation {
            callee_path: tcx.def_path_str(callee),
        };

        Mutations::new_one(mutation, smallvec![
            SubstDef::new(
                SubstLoc::Replace(expr.id, expr.span),
                Subst::AstExpr(*shadow_scope),
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
                SubstLoc::Replace(expr.id, expr.span),
                Subst::AstExpr(*default),
            ),
        ])
    }
}
