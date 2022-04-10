use rustc_ast_lowering::ResolverAstLowering;
use rustc_middle::ty::TyCtxt;

pub struct LoweringCtxt<'a> {
    pub tcx: TyCtxt<'a>,
    pub resolver: &'a dyn ResolverAstLowering,
}
