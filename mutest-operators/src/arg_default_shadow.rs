use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::res;
use mutest_emit::analysis::ty;
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Subst, SubstDef, SubstLoc};
use mutest_emit::codegen::symbols::{Ident, path};
use mutest_emit::thin_vec::thin_vec;
use mutest_emit::smallvec::{SmallVec, smallvec};

pub struct ArgDefaultShadowMutation {
    pub param_ident: Ident,
}

impl Mutation for ArgDefaultShadowMutation {
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
        let MutCtxt { tcx, resolutions: _, def_site: def, ref location } = *mcx;

        let MutLoc::FnParam(param, f) = location else { return None; };

        if param.ast.is_self() { return None; };

        let ast::PatKind::Ident(
            ast::BindingAnnotation(_, param_mutbl),
            param_ident,
            _,
        ) = param.ast.pat.kind else { return None; };

        let Some(body) = f.ast.body else { return None; };
        let Some(first_item) = body.stmts.first() else { return None; };

        let typeck = tcx.typeck_body(f.hir.body.id());

        let param_ty = typeck.pat_ty(param.hir.pat);
        if !ty::impls_trait(tcx, param_ty, res::traits::Default(tcx), vec![]) { return None; }

        // Default::default();
        let default = ast::mk::expr_call_path(def, path::default(def), thin_vec![]);

        let mutation = Self::Mutation { param_ident };

        Some((mutation, smallvec![
            SubstDef::new(
                SubstLoc::InsertBefore(first_item.id),
                // let $param: $ty = Default::default();
                Subst::AstLocal(param_ident, param_mutbl, Some(param.ast.ty.clone()), default, None),
            ),
        ]))
    }
}
