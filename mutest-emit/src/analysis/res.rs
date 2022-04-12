use crate::analysis::hir;
use crate::analysis::hir::def::{DefKind, Res};
use crate::analysis::ty::TyCtxt;
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

macro interned {
    (@STRINGIFY_PATH, $path:path) => { stringify!($path) },

    (@ITEM, trait, $ident:ident, ::$($path:ident)::+) => {
        mod $ident {
            use super::*;
            use std::lazy::SyncOnceCell;

            pub(super) static CELL: SyncOnceCell<hir::DefId> = SyncOnceCell::new();
        }

        #[doc = concat!("`", interned!(@STRINGIFY_PATH, ::$($path)::+), "`")]
        pub fn $ident(tcx: TyCtxt) -> hir::DefId {
            *$ident::CELL.get_or_init(||
                trait_def_id(tcx, &[$(Symbol::intern(stringify!($path)),)+])
                    .expect(concat!("trait ", interned!(@STRINGIFY_PATH, ::$($path)::+), " not available"))
            )
        }
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
