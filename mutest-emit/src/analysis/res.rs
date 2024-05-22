use std::hash::Hash;

use rustc_hash::FxHashSet;
use thin_vec::{ThinVec, thin_vec};

use crate::analysis::hir::{self, CRATE_DEF_ID, LOCAL_CRATE};
use crate::analysis::hir::intravisit::Visitor;
use crate::analysis::hir::def::{DefKind, Res};
use crate::analysis::ty::{self, TyCtxt};
use crate::codegen::ast;
use crate::codegen::symbols::{DUMMY_SP, Ident, Symbol, kw};

#[derive(Clone, Debug)]
pub struct ItemChild {
    pub ident: Ident,
    pub vis: ty::Visibility<hir::DefId>,
    pub res: Res,
}

pub fn item_children<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId) -> Box<dyn Iterator<Item = ItemChild> + 'tcx> {
    match tcx.def_kind(def_id) {
        DefKind::Mod | DefKind::Enum | DefKind::Trait => {
            let mod_children = match def_id.as_local() {
                Some(local_def_id) => tcx.module_children_local(local_def_id),
                None => tcx.module_children(def_id),
            };

            let iter = mod_children.iter()
                .map(|child| {
                    let res = child.res.expect_non_local();
                    ItemChild { ident: child.ident, vis: child.vis, res }
                });
            Box::new(iter)
        }
        DefKind::Impl { of_trait: _ } => {
            let iter = tcx.associated_item_def_ids(def_id).iter().copied()
                .map(move |assoc_def_id| {
                    let ident = tcx.opt_item_ident(assoc_def_id).unwrap();
                    let vis = tcx.visibility(assoc_def_id);
                    let res = Res::Def(tcx.def_kind(assoc_def_id), assoc_def_id);
                    ItemChild { ident, vis, res }
                });
            Box::new(iter)
        }
        _ => Box::new(std::iter::empty()),
    }
}

pub fn item_child_by_symbol<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, symbol: Symbol) -> Option<ItemChild> {
    item_children(tcx, def_id).find(|child| child.ident.name == symbol)
}

pub fn item_child_by_ident<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, ident: Ident) -> Option<ItemChild> {
    item_children(tcx, def_id).find(|child| child.ident == ident)
}

pub fn def_path_res<'tcx>(tcx: TyCtxt<'tcx>, path: &[Symbol]) -> Res {
    fn find_crate(tcx: TyCtxt<'_>, symbol: Symbol) -> Option<hir::DefId> {
        if tcx.crate_name(LOCAL_CRATE) == symbol { return Some(LOCAL_CRATE.as_def_id()); }

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
        .try_fold(first.res, |res, segment| {
            let def_id = res.def_id();

            if let Some(item) = item_child_by_symbol(tcx, def_id, segment) {
                Some(item.res)
            } else if matches!(res, Res::Def(DefKind::Enum | DefKind::Struct, _)) {
                tcx.inherent_impls(def_id).unwrap().iter()
                    .find_map(|&impl_def_id| item_child_by_symbol(tcx, impl_def_id, segment))
                    .map(|child| child.res)
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

pub fn def_id_path<'tcx>(tcx: TyCtxt<'tcx>, mut def_id: hir::DefId) -> Vec<hir::DefId> {
    let mut path = vec![def_id];
    while let Some(parent) = tcx.opt_parent(def_id) {
        path.push(parent);
        def_id = parent;
    }
    path.reverse();

    path
}

pub fn def_hir_path<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::LocalDefId) -> Vec<(hir::HirId, hir::Node)> {
    let def_hir_id = tcx.local_def_id_to_hir_id(def_id);

    let mut path = tcx.hir().parent_iter(def_hir_id).collect::<Vec<_>>();
    path.reverse();

    let def_node = tcx.hir_node(def_hir_id);
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

pub fn callee<'tcx>(typeck: &'tcx ty::TypeckResults<'tcx>, expr: &'tcx hir::Expr<'tcx>) -> Option<(hir::DefId, ty::GenericArgsRef<'tcx>)> {
    match expr.kind {
        hir::ExprKind::Call(expr, _) => {
            let &ty::TyKind::FnDef(def_id, generic_args) = typeck.node_type(expr.hir_id).kind() else { return None; };
            Some((def_id, generic_args))
        }
        hir::ExprKind::MethodCall(_, _, _, _) => {
            let Some(def_id) = typeck.type_dependent_def_id(expr.hir_id) else { return None; };
            let generic_args = typeck.node_args(expr.hir_id);
            Some((def_id, generic_args))
        }
        _ => None,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Call<'tcx> {
    pub def_id: hir::DefId,
    pub generic_args: ty::GenericArgsRef<'tcx>,
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
        if let Some((def_id, generic_args)) = callee(self.typeck, expr) {
            self.callees.push(Call {
                def_id,
                generic_args,
                unsafety: self.current_scope_unsafety,
            });
        }

        hir::intravisit::walk_expr(self, expr);
    }
}

pub fn collect_callees<'tcx>(tcx: TyCtxt<'tcx>, body: &'tcx hir::Body<'tcx>) -> Vec<Call<'tcx>> {
    let typeck = tcx.typeck_body(body.id());

    let body_owner = tcx.hir_node(tcx.hir().body_owner(body.id()));
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

pub fn instantiate_generic_args<'tcx, T>(tcx: TyCtxt<'tcx>, foldable: T, generic_args: ty::GenericArgsRef<'tcx>) -> T
where
    T: ty::TypeFoldable<TyCtxt<'tcx>>,
{
    ty::EarlyBinder::bind(foldable).instantiate(tcx, generic_args)
}

#[derive(Clone, Debug)]
pub struct DefPathSegment {
    pub def_id: hir::DefId,
    pub ident: Ident,
}

#[derive(Clone, Debug)]
pub struct DefPath {
    pub segments: Vec<DefPathSegment>,
}

impl DefPath {
    pub fn def_id_path(&self) -> impl Iterator<Item = hir::DefId> + '_ {
        self.segments.iter().map(|segment| segment.def_id)
    }

    pub fn ast_path(&self) -> ast::Path {
        let segments = self.segments.iter().map(|segment| {
            ast::PathSegment { id: ast::DUMMY_NODE_ID, ident: segment.ident, args: None }
        }).collect::<ThinVec<_>>();

        ast::Path { span: DUMMY_SP, segments, tokens: None }
    }
}

pub fn visible_def_paths<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, scope: Option<hir::DefId>) -> ThinVec<DefPath> {
    let (root_def_id, root_ident) = match def_id.as_local() {
        Some(_) => (CRATE_DEF_ID.to_def_id(), Ident::new(kw::Crate, DUMMY_SP)),
        None => (def_id.krate.as_def_id(), Ident::new(tcx.crate_name(def_id.krate), DUMMY_SP)),
    };
    let root_def_path = DefPath { segments: vec![DefPathSegment { def_id: root_def_id, ident: root_ident }] };

    if root_def_id == def_id {
        return thin_vec![root_def_path];
    }

    let mut paths = thin_vec![];

    let mut seen_containers: FxHashSet<hir::DefId> = Default::default();
    let mut worklist = vec![(root_def_id, root_def_path)];
    while !worklist.is_empty() {
        let mut new_worklist: Vec<(hir::DefId, DefPath)> = vec![];

        for (container_def_id, container_def_path) in worklist.drain(..) {
            let children = item_children(tcx, container_def_id);

            for child in children {
                let visible = false
                    || child.vis == ty::Visibility::Public
                    || child.vis == ty::Visibility::Restricted(CRATE_DEF_ID.to_def_id())
                    || scope.is_some_and(|scope| child.vis.is_accessible_from(scope, tcx));

                if !visible { continue; }
                if child.ident.name == kw::Underscore { continue; }

                if child.res.opt_def_id() == Some(def_id) {
                    let mut path = container_def_path.clone();
                    path.segments.push(DefPathSegment { def_id, ident: child.ident });
                    paths.push(path);
                }

                match child.res {
                    Res::Def(DefKind::Mod | DefKind::Enum | DefKind::Trait | DefKind::Impl { .. }, child_def_id) => {
                        if seen_containers.contains(&child_def_id) { continue; }
                        seen_containers.insert(child_def_id);

                        let mut path = container_def_path.clone();
                        path.segments.push(DefPathSegment { def_id: child_def_id, ident: child.ident });
                        new_worklist.push((child_def_id, path));
                    }
                    _ => {}
                };
            }
        }

        worklist.extend(new_worklist);
    }

    paths
}

pub fn visible_paths<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, scope: Option<hir::DefId>) -> ThinVec<ast::Path> {
    visible_def_paths(tcx, def_id, scope).into_iter().map(|path| path.ast_path()).collect::<ThinVec<_>>()
}

macro interned {
    (@STRINGIFY_PATH, $path:path) => { stringify!($path) },

    (@ITEM_IMPL, $kind_fn:ident, $kind_display_name:expr, $ident:ident, ::$($path:ident)::+) => {
        mod $ident {
            use super::*;
            use std::sync::OnceLock;

            pub(super) static CELL: OnceLock<hir::DefId> = OnceLock::new();
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
