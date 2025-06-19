use mutest_emit::{Mutation, Operator};
use mutest_emit::analysis::res;
use mutest_emit::analysis::ty;
use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutCtxt, MutLoc, Mutations, Subst, SubstDef, SubstLoc};
use mutest_emit::codegen::symbols::{Ident, path};
use mutest_emit::thin_vec::thin_vec;
use mutest_emit::smallvec::{SmallVec, smallvec};

fn find_ident_pats<'ast>(pat: &'ast ast::Pat) -> Vec<&'ast ast::Pat> {
    fn find_ident_pats_impl<'ast>(pat: &'ast ast::Pat, ident_pats: &mut Vec<&'ast ast::Pat>) {
        if let ast::PatKind::Ident(..) = &pat.kind {
            ident_pats.push(pat);
        }

        match &pat.kind {
            | ast::PatKind::Missing
            | ast::PatKind::Wild
            | ast::PatKind::Never
            | ast::PatKind::Ident(_, _, None)
            | ast::PatKind::Path(_, _)
            | ast::PatKind::Rest
            | ast::PatKind::Range(_, _, _)
            | ast::PatKind::Expr(_)
            | ast::PatKind::MacCall(_)
            | ast::PatKind::Err(_)
            => {}

            | ast::PatKind::Paren(inner_pat)
            | ast::PatKind::Ident(_, _, Some(inner_pat))
            | ast::PatKind::Box(inner_pat)
            | ast::PatKind::Ref(inner_pat, _)
            | ast::PatKind::Deref(inner_pat)
            | ast::PatKind::Guard(inner_pat, _)
            => find_ident_pats_impl(inner_pat, ident_pats),

            | ast::PatKind::Tuple(pats)
            | ast::PatKind::TupleStruct(_, _, pats)
            | ast::PatKind::Or(pats)
            | ast::PatKind::Slice(pats)
            => {
                for inner_pat in pats {
                    find_ident_pats_impl(inner_pat, ident_pats);
                }
            }

            ast::PatKind::Struct(_, _, pat_fields, _) => {
                for pat_field in pat_fields {
                    find_ident_pats_impl(&pat_field.pat, ident_pats);
                }
            }
        }
    }

    let mut ident_pats = vec![];
    find_ident_pats_impl(pat, &mut ident_pats);
    ident_pats
}

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

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation> {
        let MutCtxt { opts, tcx, crate_res, def_res, def_site: def, item_hir: f_hir, body_res, location } = *mcx;

        let MutLoc::FnParam(param, f) = location else { return Mutations::none(); };

        if param.is_self() { return Mutations::none(); };

        let ident_pats = find_ident_pats(&param.pat);
        if ident_pats.is_empty() { return Mutations::none(); };

        let Some(body) = &f.fn_data.body else { return Mutations::none(); };
        let Some(first_valid_stmt) = body.stmts.iter().filter(|stmt| stmt.id != ast::DUMMY_NODE_ID).next() else { return Mutations::none(); };

        let Some(body_hir) = f_hir.body else { return Mutations::none(); };
        let typeck = tcx.typeck_body(body_hir.id());

        let mut mutations = SmallVec::with_capacity(ident_pats.len());
        for ident_pat in ident_pats {
            let ast::PatKind::Ident(
                ast::BindingMode(_, param_mutbl),
                param_ident,
                _,
            ) = ident_pat.kind else { unreachable!() };

            let Some(ident_pat_hir) = body_res.hir_pat(ident_pat) else { continue; };
            let param_ty = typeck.pat_ty(ident_pat_hir);
            if !ty::impls_trait(tcx, f_hir.owner_id.def_id, param_ty, res::traits::Default(tcx), vec![]) { continue; }

            // Short-circuit in the common case where the parameter pattern is an ident in and of itself.
            // This case is simpler, since we can simply copy the type ascription from the parameter directly.
            // NOTE: We can find out if the current pattern is the top-level ident pattern by
            //       checking if the idents are equal as they have to be unique across the function header.
            let Some(param_ty_ast) = (if let ast::PatKind::Ident(_, top_param_ident, _) = param.pat.kind && param_ident == top_param_ident {
                Some(param.ty.clone())
            } else {
                let scope = f_hir.owner_id.def_id.to_def_id();
                let def_path_handling = ty::print::DefPathHandling::PreferVisible(ty::print::ScopedItemPaths::Trimmed);
                let opaque_ty_handling = ty::print::OpaqueTyHandling::Infer;
                ty::ast_repr(tcx, crate_res, def_res, Some(scope), def, param_ty, def_path_handling, opaque_ty_handling, opts.sanitize_macro_expns, f_hir.owner_id.to_def_id())
            }) else { continue; };

            // Default::default();
            let default = ast::mk::expr_call_path(def, path::default(def), thin_vec![]);

            let mutation = Self::Mutation { param_ident };

            mutations.push((mutation, smallvec![
                SubstDef::new(
                    SubstLoc::InsertBefore(first_valid_stmt.id),
                    // let $param: $ty = Default::default();
                    Subst::AstLocal(param_ident, param_mutbl, Some(param_ty_ast), default, None),
                ),
            ]));
        }

        Mutations::new(mutations)
    }
}
