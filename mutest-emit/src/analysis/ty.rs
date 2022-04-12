pub use rustc_middle::ty::*;

use rustc_middle::ty;
use rustc_infer::infer::TyCtxtInferExt;

use crate::analysis::hir;
use crate::codegen::symbols::DUMMY_SP;

pub fn impls_trait_params<'tcx>(tcx: TyCtxt<'tcx>, param_env: ty::ParamEnv<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId) -> bool {
    tcx.infer_ctxt().enter(|infcx| {
        rustc_trait_selection::traits::type_known_to_meet_bound_modulo_regions(&infcx, param_env, ty, trait_def_id, DUMMY_SP)
    })
}

pub fn impls_trait<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>, trait_def_id: hir::DefId) -> bool {
    impls_trait_params(tcx, ty::ParamEnv::empty(), ty, trait_def_id)
}
