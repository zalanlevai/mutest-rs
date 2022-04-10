use mutest_emit::codegen::ast;
use mutest_emit::codegen::mutation::{MutLoc, Subst, SubstDef, SubstLoc, MutCtxt};
use mutest_emit::codegen::symbols::path;
use smallvec::{SmallVec, smallvec};

pub struct ReplaceArgWithDefault;

impl<'a> mutest_emit::Operator<'a> for ReplaceArgWithDefault {
    type Mutation = mutest_mutations::ReplaceArgWithDefault<'a>;

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)> {
        let MutCtxt { def_site: def, ref location } = *mcx;

        let MutLoc::FnParam(param, f) = *location else { return None; };

        let ast::PatKind::Ident(_, param_ident, _) = param.pat.kind else { return None; };

        let Some(ref body) = f.body else { return None; };
        let Some(first_item) = body.stmts.first() else { return None; };

        // let $param: $ty = Default::default();
        let default = ast::mk::expr_call_path(def, path::default(def), vec![]);
        let let_param_default = ast::mk::stmt_let(def, false, param_ident, Some(param.ty.clone()), default);

        let mutation = Self::Mutation {
            param_ident: param_ident.to_string().into(),
        };

        Some((mutation, smallvec![
            SubstDef::new(
                SubstLoc::InsertBefore(first_item.id),
                Subst::AstStmt(let_param_default),
            ),
        ]))
    }
}
