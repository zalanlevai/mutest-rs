use rustc_ast as ast;
use rustc_hir as hir;
use rustc_middle::middle::privacy::AccessLevels;
use rustc_middle::ty::{ParamEnv, TyCtxt, TypeckResults};
use rustc_span::Span;

use crate::codegen::ast_mut::AstMutation;
use crate::data_structures::arena::TypedArenaWithLookup;

pub struct EmitCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    // arena: TypedArena<SourceChange>,
    // source_changes: FxHashMap<Span, &'tcx SourceChange>,
    // arena: TypedArenaWithLookup<'tcx, Span, SourceChange>,
    arena: TypedArenaWithLookup<'tcx, ast::NodeId, AstMutation<dyn ast::AstLike>>,
}

// impl<'tcx> EmitCtxt<'tcx> {
//     fn mk_change(&'tcx mut self, change: SourceChange) {
//         let span = change.original_span;
//         let change_ref = self.arena.alloc(change);
//         self.source_changes.insert(span, change_ref);
//     }
// }

impl<'tcx> EmitCtxt<'tcx> {
    // pub(crate) fn mk_change(&'tcx mut self, change: SourceChange) -> &'tcx SourceChange {
    //     self.arena.alloc(change.original_span, change)
    // }

    // pub(crate) fn get_change(&self, span: &Span) -> Option<&'tcx SourceChange> {
    //     self.arena.get(span)
    // }

    pub(crate) fn mk_change(&'tcx mut self, change: AstMutation<dyn ast::AstLike>) -> &'tcx AstMutation<dyn ast::AstLike> {
        self.arena.alloc(change.original_span, change)
    }

    pub(crate) fn get_change(&self, span: &ast::NodeId) -> Option<&'tcx AstMutation<dyn ast::AstLike>> {
        self.arena.get(span)
    }
}

pub struct ItemCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub enclosing_body: Option<hir::BodyId>,
    pub typeck_results: Option<&'tcx TypeckResults<'tcx>>,
    pub param_env: ParamEnv<'tcx>,
    pub access_levels: &'tcx AccessLevels,
}
