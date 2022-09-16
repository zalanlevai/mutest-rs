use std::hash::Hash;

use crate::analysis::hir;
use crate::analysis::hir::intravisit::Visitor;
use crate::analysis::hir::def::{DefKind, Res};
use crate::analysis::ty::{self, Subst, TyCtxt};
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

pub fn callee<'tcx>(typeck: &'tcx ty::TypeckResults<'tcx>, expr: &'tcx hir::Expr<'tcx>) -> Option<(hir::DefId, ty::SubstsRef<'tcx>)> {
    match expr.kind {
        hir::ExprKind::Call(expr, _) => {
            let &ty::TyKind::FnDef(def_id, substs) = typeck.node_type(expr.hir_id).kind() else { return None; };
            Some((def_id, substs))
        }
        hir::ExprKind::MethodCall(_, _, _) => {
            let Some(def_id) = typeck.type_dependent_def_id(expr.hir_id) else { return None; };
            let substs = typeck.node_substs(expr.hir_id);
            Some((def_id, substs))
        }
        _ => None,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Call<'tcx> {
    pub def_id: hir::DefId,
    pub substs: ty::SubstsRef<'tcx>,
    pub unsafety: hir::Unsafety,
}

struct CalleeCollector<'tcx> {
    typeck: &'tcx ty::TypeckResults<'tcx>,
    current_scope_unsafety: hir::Unsafety,
    callees: Vec<Call<'tcx>>,
}

impl<'tcx> hir::intravisit::Visitor<'tcx> for CalleeCollector<'tcx> {
    fn visit_block(&mut self, block: &'tcx hir::Block<'tcx>) {
        let previous_scope_unsafety = self.current_scope_unsafety;
        self.current_scope_unsafety = match block.rules {
            hir::BlockCheckMode::DefaultBlock => self.current_scope_unsafety,
            hir::BlockCheckMode::UnsafeBlock(_) => hir::Unsafety::Unsafe,
        };

        hir::intravisit::walk_block(self, block);

        self.current_scope_unsafety = previous_scope_unsafety;
    }

    fn visit_expr(&mut self, expr: &'tcx hir::Expr<'tcx>) {
        if let Some((def_id, substs)) = callee(self.typeck, expr) {
            self.callees.push(Call {
                def_id,
                substs,
                unsafety: self.current_scope_unsafety,
            });
        }

        hir::intravisit::walk_expr(self, expr);
    }
}

pub fn collect_callees<'tcx>(tcx: TyCtxt<'tcx>, body: &'tcx hir::Body<'tcx>) -> Vec<Call<'tcx>> {
    let typeck = tcx.typeck_body(body.id());

    let body_owner = tcx.hir().get(tcx.hir().body_owner(body.id()));
    let body_unsafety = match body_owner {
        hir::Node::Item(item) => {
            match &item.kind {
                hir::ItemKind::Fn(sig, _, _) => sig.header.unsafety,
                _ => hir::Unsafety::Normal,
            }
        }
        hir::Node::ForeignItem(item) => {
            match &item.kind {
                hir::ForeignItemKind::Fn(_, _, _) => hir::Unsafety::Unsafe,
                _ => hir::Unsafety::Normal,
            }
        }
        hir::Node::TraitItem(item) => {
            match &item.kind {
                hir::TraitItemKind::Fn(sig, _) => sig.header.unsafety,
                _ => hir::Unsafety::Normal,
            }
        }
        hir::Node::ImplItem(item) => {
            match &item.kind {
                hir::ImplItemKind::Fn(sig, _) => sig.header.unsafety,
                _ => hir::Unsafety::Normal,
            }
        }
        _ => hir::Unsafety::Normal,
    };

    let mut collector = CalleeCollector {
        typeck,
        current_scope_unsafety: body_unsafety,
        callees: vec![],
    };
    collector.visit_body(body);

    collector.callees
}

pub fn fold_substs<'tcx, T>(tcx: TyCtxt<'tcx>, foldable: T, substs: ty::SubstsRef<'tcx>) -> T
where
    T: ty::TypeFoldable<'tcx>,
{
    ty::EarlyBinder(foldable).subst(tcx, substs)
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

        trait Add (::core::ops::Add),
        trait AddAssign (::core::ops::AddAssign),
        trait BitAnd (::core::ops::BitAnd),
        trait BitAndAssign (::core::ops::BitAndAssign),
        trait BitOr (::core::ops::BitOr),
        trait BitOrAssign (::core::ops::BitOrAssign),
        trait BitXor (::core::ops::BitXor),
        trait BitXorAssign (::core::ops::BitXorAssign),
        trait Div (::core::ops::Div),
        trait DivAssign (::core::ops::DivAssign),
        trait Mul (::core::ops::Mul),
        trait MulAssign (::core::ops::MulAssign),
        trait Rem (::core::ops::Rem),
        trait RemAssign (::core::ops::RemAssign),
        trait Shl (::core::ops::Shl),
        trait ShlAssign (::core::ops::ShlAssign),
        trait Shr (::core::ops::Shr),
        trait ShrAssign (::core::ops::ShrAssign),
        trait Sub (::core::ops::Sub),
        trait SubAssign (::core::ops::SubAssign),
    }
}

pub mod fns {
    super::interned! {
        fn default (::core::default::Default::default),
    }
}
