use rustc_ast as ast;
use rustc_hir as hir;
use rustc_middle::middle::privacy::AccessLevels;
use rustc_middle::ty::{ParamEnv, TyCtxt, TypeckResults};

pub struct EmitCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
}

pub struct ItemCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub enclosing_body: Option<hir::BodyId>,
    pub typeck_results: Option<&'tcx TypeckResults<'tcx>>,
    pub param_env: ParamEnv<'tcx>,
    pub access_levels: &'tcx AccessLevels,
}
