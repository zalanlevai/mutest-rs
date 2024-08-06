use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::res;
use mutest_emit::analysis::ty;
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use mutest_emit::codegen::symbols::{Ident, path};
use mutest_emit::thin_vec::thin_vec;
use mutest_emit::smallvec::{SmallVec, smallvec};

pub const ARG_DEFAULT_SHADOW: &str = "arg_default_shadow";

pub struct ArgDefaultShadowMutation {
    pub param_ident: Ident,
}

impl Mutation for ArgDefaultShadowMutation {
    fn op_name(&self) -> &str { ARG_DEFAULT_SHADOW }

    fn display_name(&self) -> String {
        format!("ignore `{param}` argument by shadowing it with `Default::default()`",
            param = self.param_ident
        )
    }

    fn span_label(&self) -> String {
        "ignore argument by shadowing it with `Default::default()`".to_owned()
    }
}

/// Replace the provided arguments of functions with `Default::default()` to check if each parameter
/// is tested with meaningful values.
///
/// This is done by rebinding parameters at the beginning of the function.
pub struct ArgDefaultShadow;

impl<'a> Operator<'a> for ArgDefaultShadow {
    type Mutation = ArgDefaultShadowMutation;

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
        let MutCtxt { opts: _, tcx, def_res: _, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

        let MutLoc::FnParam(param, f) = location else { return None; };

        if param.is_self() { return None; };

        let ast::PatKind::Ident(
            ast::BindingMode(_, param_mutbl),
            param_ident,
            _,
        ) = param.pat.kind else { return None; };

        let Some(body) = &f.body else { return None; };
        let Some(first_valid_stmt) = body.stmts.iter().filter(|stmt| stmt.id != ast::DUMMY_NODE_ID).next() else { return None; };

        let param_env = tcx.param_env(f_hir.owner_id.def_id);

        let Some(body_hir) = f_hir.body else { return None; };
        let typeck = tcx.typeck_body(body_hir.id());

        let Some(param_hir) = body_res.hir_param(param) else { unreachable!() };
        let param_ty = typeck.pat_ty(param_hir.pat);
        if !ty::impls_trait_with_env(tcx, param_env, param_ty, res::traits::Default(tcx), vec![]) { return None; }

        // Default::default();
        let default = ast::mk::expr_call_path(def, path::default(def), thin_vec![]);

        let mutation = Self::Mutation { param_ident };

        Some((mutation, smallvec![
            SubstDef::new(
                SubstLoc::InsertBefore(first_valid_stmt.id),
                // let $param: $ty = Default::default();
                Subst::AstLocal(param_ident, param_mutbl, Some(param.ty.clone()), default, None),
            ),
        ]))
    }
}
