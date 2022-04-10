use rustc_hir as hir;
use rustc_hir::HirId;
// use rustc_hir::intravisit::{NestedVisitorMap, Visitor};
use rustc_hir::intravisit::Visitor;
// use rustc_middle::hir::map::Map;
use rustc_span::Span;

use crate::ItemCtxt;

pub struct ItemVisitor<'tcx> {
    pub ctxt: ItemCtxt<'tcx>,
}

impl<'tcx> Visitor<'tcx> for ItemVisitor<'tcx> {
    // type Map = Map<'tcx>;
    //
    // fn nested_visit_map(&mut self) -> NestedVisitorMap<Self::Map> {
    //     NestedVisitorMap::OnlyBodies(self.ctxt.tcx.hir())
    // }

    fn visit_fn(
        &mut self,
        fk: hir::intravisit::FnKind<'tcx>,
        fd: &'tcx hir::FnDecl<'tcx>,
        b: hir::BodyId,
        s: Span,
        id: HirId,
    ) {

    }
}
