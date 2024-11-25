use std::hash::Hash;

use rustc_hash::FxHashSet;
use rustc_middle::metadata::{ModChild, Reexport};
use smallvec::{SmallVec, smallvec};
use thin_vec::ThinVec;

use crate::analysis::hir::{self, CRATE_DEF_ID, LOCAL_CRATE};
use crate::analysis::hir::intravisit::Visitor;
use crate::analysis::hir::def::{DefKind, Res};
use crate::analysis::ty::{self, Ty, TyCtxt};
use crate::codegen::ast;
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, sym, kw};

pub fn module_children<'tcx>(tcx: TyCtxt<'tcx>, mod_def_id: hir::DefId) -> &'tcx [ModChild] {
    match mod_def_id.as_local() {
        Some(mod_local_def_id) => tcx.module_children_local(mod_local_def_id),
        None => tcx.module_children(mod_def_id),
    }
}

pub fn lookup_mod_child<'tcx>(tcx: TyCtxt<'tcx>, mod_def_id: hir::DefId, res: hir::Res<!>, name: Symbol) -> Option<&'tcx ModChild> {
    module_children(tcx, mod_def_id).into_iter()
        .find(|mod_child| mod_child.res == res && mod_child.ident.name == name)
}

#[derive(Clone, Debug)]
pub struct ItemChild {
    pub ident: Ident,
    pub vis: ty::Visibility<hir::DefId>,
    pub res: Res,
    pub reexport: Option<Reexport>,
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
                    let reexport = 'reexport: {
                        let [reexport, ..] = &child.reexport_chain[..] else { break 'reexport None; };
                        match reexport {
                            Reexport::Single(_) => Some(*reexport),
                            Reexport::Glob(_) => None,
                            Reexport::ExternCrate(_) => Some(*reexport),
                            Reexport::MacroUse | Reexport::MacroExport => Some(*reexport),
                        }
                    };
                    ItemChild { ident: child.ident, vis: child.vis, res, reexport }
                });
            Box::new(iter)
        }
        DefKind::Impl { of_trait: _ } => {
            let iter = tcx.associated_item_def_ids(def_id).iter().copied()
                .map(move |assoc_def_id| {
                    let ident = tcx.opt_item_ident(assoc_def_id).unwrap();
                    let vis = tcx.visibility(assoc_def_id);
                    let res = Res::Def(tcx.def_kind(assoc_def_id), assoc_def_id);
                    ItemChild { ident, vis, res, reexport: None }
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

pub fn parent_iter<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId) -> DefIdParentIter<'tcx> {
    DefIdParentIter { tcx, def_id }
}

pub struct DefIdParentIter<'tcx> {
    tcx: TyCtxt<'tcx>,
    def_id: hir::DefId,
}

impl<'tcx> std::iter::Iterator for DefIdParentIter<'tcx> {
    type Item = hir::DefId;

    fn next(&mut self) -> Option<Self::Item> {
        let parent_def_id = self.tcx.opt_parent(self.def_id)?;
        self.def_id = parent_def_id;
        Some(parent_def_id)
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

#[derive(Copy, Clone, Debug)]
pub enum DefPathRootTy {
    Bool,
    Char,
    Int(ty::IntTy),
    Uint(ty::UintTy),
    Float(ty::FloatTy),
    Str,
}

impl DefPathRootTy {
    pub fn from_ty<'tcx>(ty: Ty<'tcx>) -> Option<Self> {
        match ty.kind() {
            ty::TyKind::Bool => Some(Self::Bool),
            ty::TyKind::Char => Some(Self::Char),
            ty::TyKind::Int(int_ty) => Some(Self::Int(*int_ty)),
            ty::TyKind::Uint(uint_ty) => Some(Self::Uint(*uint_ty)),
            ty::TyKind::Float(float_ty) => Some(Self::Float(*float_ty)),
            ty::TyKind::Str => Some(Self::Str),
            _ => None,
        }
    }

    pub fn as_ident(&self, sp: Span) -> Ident {
        match self {
            Self::Bool => Ident::new(sym::bool, sp),
            Self::Char => Ident::new(sym::char, sp),
            Self::Int(ty::IntTy::I8) => Ident::new(sym::i8, sp),
            Self::Int(ty::IntTy::I16) => Ident::new(sym::i16, sp),
            Self::Int(ty::IntTy::I32) => Ident::new(sym::i32, sp),
            Self::Int(ty::IntTy::I64) => Ident::new(sym::i64, sp),
            Self::Int(ty::IntTy::I128) => Ident::new(sym::i128, sp),
            Self::Int(ty::IntTy::Isize) => Ident::new(sym::isize, sp),
            Self::Uint(ty::UintTy::U8) => Ident::new(sym::u8, sp),
            Self::Uint(ty::UintTy::U16) => Ident::new(sym::u16, sp),
            Self::Uint(ty::UintTy::U32) => Ident::new(sym::u32, sp),
            Self::Uint(ty::UintTy::U64) => Ident::new(sym::u64, sp),
            Self::Uint(ty::UintTy::U128) => Ident::new(sym::u128, sp),
            Self::Uint(ty::UintTy::Usize) => Ident::new(sym::usize, sp),
            Self::Float(ty::FloatTy::F16) => Ident::new(sym::f16, sp),
            Self::Float(ty::FloatTy::F32) => Ident::new(sym::f32, sp),
            Self::Float(ty::FloatTy::F64) => Ident::new(sym::f64, sp),
            Self::Float(ty::FloatTy::F128) => Ident::new(sym::f128, sp),
            Self::Str => Ident::new(sym::str, sp),
        }
    }
}

#[derive(Clone, Debug)]
pub struct DefPathSegment {
    pub def_id: hir::DefId,
    pub ident: Ident,
    pub reexport: Option<Reexport>,
}

#[derive(Clone, Debug)]
pub struct DefPath {
    pub type_root: Option<DefPathRootTy>,
    pub segments: Vec<DefPathSegment>,
    pub global: bool,
}

impl DefPath {
    pub fn from_def_id<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId) -> Option<Self> {
        let segments = def_id_path(tcx, def_id).into_iter()
            .map(|def_id| {
                let span = tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
                let name = tcx.opt_item_name(def_id)?;
                Some(DefPathSegment { def_id, ident: Ident::new(name, span), reexport: None })
            })
            .try_collect::<Vec<_>>()?;
        Some(Self { type_root: None, segments, global: true })
    }

    pub fn def_id_path(&self) -> impl Iterator<Item = hir::DefId> + '_ {
        self.segments.iter().map(|segment| segment.def_id)
    }

    pub fn ast_path(&self) -> ast::Path {
        let mut global = self.global;

        let mut segments = self.segments.iter().map(|segment| {
            let mut ident = segment.ident;

            if segment.def_id == LOCAL_CRATE.as_def_id() {
                ident = Ident::new(kw::Crate, DUMMY_SP);
                // We must not make the path global if we use the `crate` keyword.
                global = false;
            }

            ast::PathSegment { id: ast::DUMMY_NODE_ID, ident, args: None }
        }).collect::<ThinVec<_>>();

        if let Some(root_ty) = self.type_root {
            let ident = root_ty.as_ident(DUMMY_SP);
            segments.insert(0, ast::PathSegment { id: ast::DUMMY_NODE_ID, ident, args: None });
            global = false;
        }

        if global {
            segments.insert(0, ast::PathSegment::path_root(DUMMY_SP));
        }

        ast::Path { span: DUMMY_SP, segments, tokens: None }
    }
}

fn inherent_impl_ty_to_def_id<'tcx>(impl_ty: Ty<'tcx>) -> hir::DefId {
    match impl_ty.kind() {
        ty::TyKind::Adt(adt_def, _) => adt_def.did(),
        _ => unreachable!("inherent impl is for unhandled type {impl_ty:?}"),
    }
}

fn concretize_inherent_impl_in_def_path_segment<'tcx>(tcx: TyCtxt<'tcx>, segment: &mut DefPathSegment) {
    let impl_subject = tcx.impl_subject(segment.def_id);
    let ty::ImplSubject::Inherent(implementer_ty) = impl_subject.instantiate_identity() else { unreachable!("encountered trait impl in def path") };

    let implementer_def_id = inherent_impl_ty_to_def_id(implementer_ty);

    segment.def_id = implementer_def_id;
    segment.ident = tcx.opt_item_ident(implementer_def_id).unwrap();
}

pub fn relative_def_path<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, scope: hir::DefId) -> Option<DefPath> {
    if !tcx.is_descendant_of(def_id, scope) { return None; }

    if def_id == scope {
        let span = tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
        let name = tcx.opt_item_name(def_id)?;
        return Some(DefPath { type_root: None, segments: vec![DefPathSegment { def_id, ident: Ident::new(name, span), reexport: None }], global: false });
    }

    let full_def_id_path = def_id_path(tcx, def_id);
    let scope_def_id_path = def_id_path(tcx, scope);
    let relative_def_id_path = &full_def_id_path[scope_def_id_path.len()..];

    let mut def_path = DefPath { type_root: None, segments: Vec::with_capacity(relative_def_id_path.len()), global: false };
    for &def_id in relative_def_id_path {
        let span = tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
        let name = tcx.opt_item_name(def_id).unwrap_or(kw::Empty);
        def_path.segments.push(DefPathSegment { def_id, ident: Ident::new(name, span), reexport: None });
    }

    for segment in &mut def_path.segments {
        let hir::DefKind::Impl { of_trait: _ } = tcx.def_kind(segment.def_id) else { continue; };
        concretize_inherent_impl_in_def_path_segment(tcx, segment);
    }

    Some(def_path)
}

pub fn locally_visible_def_path<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, mut scope: hir::DefId) -> Result<DefPath, hir::DefId> {
    if !tcx.is_descendant_of(def_id, scope) {
        'fail: {
            // For some scopes, we can make an adjustment and try to find a relative path from the parent scope.
            let is_transparent = |def_kind: hir::DefKind| matches!(def_kind,
                | hir::DefKind::Struct | hir::DefKind::Enum | hir::DefKind::Union | hir::DefKind::Variant | hir::DefKind::TyAlias
                | hir::DefKind::ForeignMod | hir::DefKind::ForeignTy
                | hir::DefKind::Trait | hir::DefKind::Impl { .. } | hir::DefKind::TraitAlias
                | hir::DefKind::Fn | hir::DefKind::Const | hir::DefKind::Static { .. } | hir::DefKind::Ctor(..)
                | hir::DefKind::AssocTy | hir::DefKind::AssocFn | hir::DefKind::AssocConst
                | hir::DefKind::AnonConst
            );
            while is_transparent(tcx.def_kind(scope)) && let Some(parent_scope) = tcx.opt_parent(scope) {
                scope = parent_scope;
                // Adjustment succeeded, escape failing case.
                if tcx.is_descendant_of(def_id, parent_scope) { break 'fail; }
            }

            return Err(scope);
        }
    }

    // Ensure that traits are still named when referring to assoc items.
    if let hir::DefKind::Trait | hir::DefKind::TraitAlias | hir::DefKind::Impl { .. } = tcx.def_kind(scope) {
        scope = tcx.parent(scope);
    }

    let mut def_path = relative_def_path(tcx, def_id, scope).unwrap();

    if let hir::DefKind::Impl { of_trait: _ } = tcx.def_kind(scope) {
        let ident = Ident::new(kw::SelfUpper, DUMMY_SP);
        def_path.segments.insert(0, DefPathSegment { def_id: scope, ident, reexport: None });
    }

    Ok(def_path)
}

pub fn visible_def_paths<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, scope: Option<hir::DefId>, ignore_reexport: Option<hir::DefId>) -> SmallVec<[DefPath; 1]> {
    let mut impl_parents = parent_iter(tcx, def_id).enumerate().filter(|(_, def_id)| matches!(tcx.def_kind(def_id), hir::DefKind::Impl { of_trait: _ }));
    match impl_parents.next() {
        // `..::{impl#?}::$assoc_item::..` path.
        // NOTE: Such paths will never be accessible outside of the scope of the assoc item.
        Some((1.., _)) => { return smallvec![]; }

        // `..::{impl#?}::$assoc_item` path.
        Some((0, impl_parent_def_id)) => {
            let impl_subject = tcx.impl_subject(impl_parent_def_id);
            let ty::ImplSubject::Inherent(implementer_ty) = impl_subject.instantiate_identity() else { unreachable!("encountered trait impl in def path") };

            let mut visible_root_def_paths = match DefPathRootTy::from_ty(implementer_ty) {
                Some(type_root) => smallvec![DefPath { type_root: Some(type_root), segments: vec![], global: false }],
                None => {
                    let implementer_def_id = inherent_impl_ty_to_def_id(implementer_ty);
                    visible_def_paths(tcx, implementer_def_id, scope, ignore_reexport)
                }
            };

            if visible_root_def_paths.is_empty() { return smallvec![]; }

            for visible_root_def_path in &mut visible_root_def_paths {
                visible_root_def_path.segments.push(DefPathSegment {
                    def_id: def_id,
                    ident: tcx.opt_item_ident(def_id).unwrap(),
                    reexport: None,
                });
            }

            return visible_root_def_paths;
        }

        None => {}
    }

    let extern_crates = tcx.hir_crate_items(()).definitions()
        .filter(|&def_id| matches!(tcx.def_kind(def_id), hir::DefKind::ExternCrate))
        .filter_map(|def_id| {
            let Some(cnum) = tcx.extern_mod_stmt_cnum(def_id) else { return None; };
            Some((def_id, cnum))
        });

    let mut extern_prelude = tcx.sess.opts.externs.iter()
        .filter(|(_, entry)| entry.add_prelude)
        .map(|(name, _)| Symbol::intern(name))
        .collect::<FxHashSet<_>>();
    if !tcx.hir().krate_attrs().iter().any(|attr| ast::inspect::is_word_attr(attr, None, sym::no_core)) {
        extern_prelude.insert(sym::core);
        if !tcx.hir().krate_attrs().iter().any(|attr| ast::inspect::is_word_attr(attr, None, sym::no_std)) {
            extern_prelude.insert(sym::alloc);
            extern_prelude.insert(sym::std);
        }
    }

    let (root_def_id, root_def_path) = match def_id.as_local() {
        Some(_) => {
            let root_def_id = CRATE_DEF_ID.to_def_id();
            let root_ident = Ident::new(kw::Crate, DUMMY_SP);
            let root_def_path = DefPath { type_root: None, segments: vec![DefPathSegment { def_id: root_def_id, ident: root_ident, reexport: None }], global: true };
            (root_def_id, root_def_path)
        }
        None if let crate_name = tcx.crate_name(def_id.krate) && extern_prelude.contains(&crate_name) => {
            let root_def_id = def_id.krate.as_def_id();
            let root_ident = Ident::new(crate_name, DUMMY_SP);
            let root_def_path = DefPath { type_root: None, segments: vec![DefPathSegment { def_id: root_def_id, ident: root_ident, reexport: None }], global: true };
            (root_def_id, root_def_path)
        }
        None => 'root: {
            let mut reachable_extern_crates = extern_crates
                .filter(|&(_extern_crate_def_id, cnum)| cnum == def_id.krate)
                .filter(|&(extern_crate_def_id, _cnum)| {
                    false
                        || tcx.opt_local_parent(extern_crate_def_id) == Some(CRATE_DEF_ID)
                        || scope.is_some_and(|scope| tcx.is_descendant_of(extern_crate_def_id.to_def_id(), scope))
                });
            let Some((reachable_extern_crate_def_id, _cnum)) = reachable_extern_crates.next() else {
                let visible_parent_map = tcx.visible_parent_map(());
                let mut visible_def_id = def_id;
                while let Some(&visible_parent) = visible_parent_map.get(&visible_def_id) {
                    visible_def_id = visible_parent;
                    if let Some(cnum) = visible_def_id.as_crate_root() {
                        let crate_name = tcx.crate_name(cnum);
                        if !extern_prelude.contains(&crate_name) { continue; }

                        let root_ident = Ident::new(tcx.crate_name(cnum), DUMMY_SP);
                        let root_def_path = DefPath { type_root: None, segments: vec![DefPathSegment { def_id: visible_def_id, ident: root_ident, reexport: None }], global: true };
                        break 'root (visible_def_id, root_def_path);
                    }
                }

                panic!("expected {def_id:?} to be reached through another crate in the extern prelude");
            };

            let root_def_id = def_id.krate.as_def_id();
            let Some(mut root_def_path) = DefPath::from_def_id(tcx, reachable_extern_crate_def_id.to_def_id()) else { return smallvec![] };

            if let Some(scope) = scope && tcx.is_descendant_of(reachable_extern_crate_def_id.to_def_id(), scope) {
                let scope_def_id_path = def_id_path(tcx, scope);
                root_def_path.segments.splice(0..scope_def_id_path.len(), []);
                root_def_path.global = false;
            }

            (root_def_id, root_def_path)
        }
    };

    if root_def_id == def_id {
        return smallvec![root_def_path];
    }

    let mut paths = smallvec![];

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
                if let Some(reexport) = child.reexport && reexport.id() == ignore_reexport { continue; }

                if child.res.opt_def_id() == Some(def_id) {
                    let mut path = container_def_path.clone();
                    path.segments.push(DefPathSegment { def_id, ident: child.ident, reexport: child.reexport });
                    paths.push(path);
                }

                match child.res {
                    Res::Def(DefKind::Mod | DefKind::Enum | DefKind::Trait | DefKind::Impl { .. }, child_def_id) => {
                        if seen_containers.contains(&child_def_id) { continue; }
                        seen_containers.insert(child_def_id);

                        let mut path = container_def_path.clone();
                        path.segments.push(DefPathSegment { def_id: child_def_id, ident: child.ident, reexport: child.reexport });
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

pub fn visible_paths<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, scope: Option<hir::DefId>, ignore_reexport: Option<hir::DefId>) -> SmallVec<[ast::Path; 1]> {
    visible_def_paths(tcx, def_id, scope, ignore_reexport).into_iter().map(|path| path.ast_path()).collect::<SmallVec<_>>()
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
