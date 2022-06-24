use crate::analysis::hir;
use crate::analysis::hir::intravisit::Visitor;
use crate::analysis::hir::def::{DefKind, Res};
use crate::analysis::ty::{self, TyCtxt, TypeFoldable};
use crate::codegen::symbols::Symbol;

pub fn def_path_res<'tcx>(tcx: TyCtxt<'tcx>, path: &[Symbol]) -> Res {
    fn item_child_by_symbol(tcx: TyCtxt<'_>, def_id: hir::DefId, symbol: Symbol) -> Option<Res> {
        match tcx.def_kind(def_id) {
            DefKind::Mod | DefKind::Enum | DefKind::Trait => {
                tcx.module_children(def_id).iter()
                    .find(|item| item.ident.name == symbol)
                    .map(|child| child.res.expect_non_local())
            }
            DefKind::Impl => {
                tcx.associated_item_def_ids(def_id).iter().copied()
                    .find(|assoc_def_id| tcx.item_name(*assoc_def_id) == symbol)
                    .map(|assoc_def_id| Res::Def(tcx.def_kind(assoc_def_id), assoc_def_id))
            }
            _ => None,
        }
    }

    fn find_crate(tcx: TyCtxt<'_>, symbol: Symbol) -> Option<hir::DefId> {
        tcx.crates(()).iter().copied()
            .find(|&num| tcx.crate_name(num) == symbol)
            .map(hir::CrateNum::as_def_id)
    }

    let (base, first, path) = match *path {
        [base, first, ref path @ ..] => (base, first, path),
        [primitive] => return hir::PrimTy::from_name(primitive).map_or(Res::Err, Res::PrimTy),
        _ => return Res::Err,
    };

    let Some(first) = find_crate(tcx, base).and_then(|id| item_child_by_symbol(tcx, id, first)) else { return Res::Err };

    path.iter().copied()
        .try_fold(first, |res, segment| {
            let def_id = res.def_id();

            if let Some(item) = item_child_by_symbol(tcx, def_id, segment) {
                Some(item)
            } else if matches!(res, Res::Def(DefKind::Enum | DefKind::Struct, _)) {
                tcx.inherent_impls(def_id).iter()
                    .find_map(|&impl_def_id| item_child_by_symbol(tcx, impl_def_id, segment))
            } else {
                None
            }
        })
        .unwrap_or(Res::Err)
}

pub fn trait_def_id<'tcx>(tcx: TyCtxt<'tcx>, path: &[Symbol]) -> Option<hir::DefId> {
    match def_path_res(tcx, path) {
        Res::Def(DefKind::Trait | DefKind::TraitAlias, trait_id) => Some(trait_id),
        _ => None,
    }
}

pub fn fn_def_id<'tcx>(tcx: TyCtxt<'tcx>, path: &[Symbol]) -> Option<hir::DefId> {
    match def_path_res(tcx, path) {
        Res::Def(DefKind::Fn | DefKind::AssocFn, trait_id) => Some(trait_id),
        _ => None,
    }
}

pub fn def_hir_path<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::LocalDefId) -> Vec<(hir::HirId, hir::Node)> {
    let def_hir_id = tcx.hir().local_def_id_to_hir_id(def_id);

    let mut path = tcx.hir().parent_iter(def_hir_id).collect::<Vec<_>>();
    path.reverse();

    let def_node = tcx.hir().get(def_hir_id);
    path.push((def_hir_id, def_node));

    path
}

pub fn qpath_res<'tcx>(typeck: &'tcx ty::TypeckResults<'tcx>, qpath: &'tcx hir::QPath<'tcx>, id: hir::HirId) -> Res {
    match qpath {
        hir::QPath::Resolved(_, path) => path.res,
        hir::QPath::TypeRelative(..) | hir::QPath::LangItem(..) => {
            typeck.type_dependent_def(id)
                .map_or(Res::Err, |(kind, def_id)| Res::Def(kind, def_id))
        }
    }
}

pub fn callee<'tcx>(typeck: &'tcx ty::TypeckResults<'tcx>, expr: &'tcx hir::Expr<'tcx>) -> Option<(hir::DefId, Option<ty::SubstsRef<'tcx>>)> {
    fn callee_def_id<'tcx>(typeck: &'tcx ty::TypeckResults<'tcx>, expr: &'tcx hir::Expr<'tcx>) -> Option<hir::DefId> {
        match expr.kind {
            hir::ExprKind::Call(expr, _) => {
                if let hir::ExprKind::Path(qpath) = &expr.kind
                    && let Some(def_id) = qpath_res(typeck, qpath, expr.hir_id).opt_def_id()
                {
                    return Some(def_id);
                }
            }
            hir::ExprKind::MethodCall(_, _, _) => {
                if let Some(def_id) = typeck.type_dependent_def_id(expr.hir_id) {
                    return Some(def_id);
                }
            }
            _ => {}
        };

        None
    }

    let Some(def_id) = callee_def_id(typeck, expr) else { return None; };

    let substs = match typeck.node_substs(expr.hir_id) {
        substs if substs.needs_subst() => None,
        substs => Some(substs),
    };

    Some((def_id, substs))
}

struct CalleeCollector<'tcx> {
    typeck: &'tcx ty::TypeckResults<'tcx>,
    callees: Vec<(hir::DefId, Option<ty::SubstsRef<'tcx>>)>,
}

impl<'tcx> hir::intravisit::Visitor<'tcx> for CalleeCollector<'tcx> {
    fn visit_expr(&mut self, expr: &'tcx hir::Expr<'tcx>) {
        if let Some(callee) = callee(self.typeck, expr) {
            self.callees.push(callee);
        }

        hir::intravisit::walk_expr(self, expr);
    }
}

pub fn collect_callees<'tcx>(tcx: TyCtxt<'tcx>, body: &'tcx hir::Body) -> Vec<(hir::DefId, Option<ty::SubstsRef<'tcx>>)> {
    let typeck = tcx.typeck_body(body.id());

    let mut collector = CalleeCollector {
        typeck,
        callees: vec![],
    };
    collector.visit_body(body);

    collector.callees
}

macro interned {
    (@STRINGIFY_PATH, $path:path) => { stringify!($path) },

    (@ITEM_IMPL, $kind_fn:ident, $kind_display_name:expr, $ident:ident, ::$($path:ident)::+) => {
        mod $ident {
            use super::*;
            use std::lazy::SyncOnceCell;

            pub(super) static CELL: SyncOnceCell<hir::DefId> = SyncOnceCell::new();
        }

        #[doc = concat!("`", interned!(@STRINGIFY_PATH, ::$($path)::+), "`")]
        pub fn $ident(tcx: TyCtxt) -> hir::DefId {
            *$ident::CELL.get_or_init(||
                $kind_fn(tcx, &[$(Symbol::intern(stringify!($path)),)+])
                    .expect(concat!($kind_display_name, " ", interned!(@STRINGIFY_PATH, ::$($path)::+), " not available"))
            )
        }
    },

    (@ITEM, trait, $ident:ident, ::$($path:ident)::+) => {
        interned!(@ITEM_IMPL, trait_def_id, "trait", $ident, ::$($path)::+);
    },
    (@ITEM, fn, $ident:ident, ::$($path:ident)::+) => {
        interned!(@ITEM_IMPL, fn_def_id, "function", $ident, ::$($path)::+);
    },

    ($($kind:tt $ident:ident (::$($path:ident)::+)),* $(,)?) => {
        $(
            interned!(@ITEM, $kind, $ident, ::$($path)::+);
        )*
    },
}

#[allow(non_snake_case)]
pub mod traits {
    super::interned! {
        trait Default (::core::default::Default),
    }
}

pub mod fns {
    super::interned! {
        fn default (::core::default::Default::default)
    }
}
