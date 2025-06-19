use std::num::NonZeroUsize;

use rustc_hash::{FxHashMap, FxHashSet};
use rustc_middle::span_bug;
use rustc_middle::metadata::{ModChild, Reexport};
use rustc_session::config::ExternLocation;
use rustc_session::search_paths::PathKind;
use smallvec::{SmallVec, smallvec};
use thin_vec::ThinVec;

use crate::analysis::call_graph::{Call, CallKind};
use crate::analysis::hir::{self, CRATE_DEF_ID, LOCAL_CRATE};
use crate::analysis::hir::intravisit::Visitor;
use crate::analysis::hir::def::{DefKind, Res};
use crate::analysis::ty::{self, Ty, TyCtxt};
use crate::codegen::ast::{self, P};
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, sym, kw};

pub struct CrateResolutions<'tcx> {
    tcx: TyCtxt<'tcx>,
    extern_crate_name_to_cnum: FxHashMap<Symbol, Option<hir::CrateNum>>,
    cnum_to_extern_crate_name: FxHashMap<hir::CrateNum, Symbol>,
}

impl<'tcx> CrateResolutions<'tcx> {
    pub fn from_post_analysis_tcx(tcx: TyCtxt<'tcx>) -> Self {
        let crate_sources = tcx.crates(()).iter()
            .filter_map(|&cnum| {
                let crate_source = tcx.used_crate_source(cnum);
                let extern_crate_source_paths = [&crate_source.dylib, &crate_source.rlib, &crate_source.rmeta].into_iter().flatten()
                    .filter(|(_, path_kind)| matches!(path_kind, PathKind::ExternFlag))
                    .map(|(path, _)| path.clone())
                    .collect::<SmallVec<[_; 3]>>();
                match &extern_crate_source_paths[..] {
                    [] => None,
                    _ => Some((cnum, extern_crate_source_paths)),
                }
            })
            .collect::<FxHashMap<_, _>>();

        let mut extern_crate_name_to_cnum = tcx.sess.opts.externs.iter()
            .filter(|(_, entry)| entry.add_prelude)
            .map(|(name, entry)| {
                let renamed_cnum = match &entry.location {
                    // --extern name
                    ExternLocation::FoundInLibrarySearchDirectories => None,

                    // --extern name=file.rlib
                    ExternLocation::ExactPaths(possible_paths) => 'arm: {
                        // HACK: Find crate with sources matching this --extern flag.
                        let Some((&cnum, _)) = crate_sources.iter().find(|(_, source_paths)| {
                            source_paths.iter().any(|source_path| possible_paths.iter().any(|possible_path| {
                                possible_path.canonicalized() == source_path
                            }))
                        }) else {
                            // NOTE: We can only fetch the crate sources for actually used crates,
                            //       so we can safely discard unused crates without used sources.
                            // TODO: It might be better to remove such crates entirely from the
                            //       apparent extern prelude, as that is what rustc seems to do.
                            break 'arm None;
                        };
                        Some(cnum)
                    }
                };

                (Symbol::intern(name), renamed_cnum)
            })
            .collect::<FxHashMap<_, _>>();
        if !tcx.hir_krate_attrs().iter().any(|attr| hir::attr::is_word_attr(attr, None, sym::no_core)) {
            extern_crate_name_to_cnum.insert(sym::core, None);
            if !tcx.hir_krate_attrs().iter().any(|attr| hir::attr::is_word_attr(attr, None, sym::no_std)) {
                extern_crate_name_to_cnum.insert(sym::alloc, None);
                extern_crate_name_to_cnum.insert(sym::std, None);
            }
        }

        let cnum_to_extern_crate_name = extern_crate_name_to_cnum.iter()
            .filter_map(|(crate_name, cnum)| cnum.map(|cnum| (cnum, *crate_name)))
            .collect::<FxHashMap<_, _>>();

        Self {
            tcx,
            extern_crate_name_to_cnum,
            cnum_to_extern_crate_name,
        }
    }

    pub fn crate_by_visible_name(&self, symbol: Symbol) -> Option<hir::CrateNum> {
        self.extern_crate_name_to_cnum.get(&symbol).copied().flatten().or_else(|| {
            self.tcx.crates(()).into_iter().find(|&&cnum| self.tcx.crate_name(cnum) == symbol).copied()
        })
    }

    pub fn visible_crate_name(&self, cnum: hir::CrateNum) -> Symbol {
        self.cnum_to_extern_crate_name.get(&cnum).copied().unwrap_or_else(|| self.tcx.crate_name(cnum))
    }

    pub fn is_in_extern_prelude(&self, symbol: Symbol) -> bool {
        self.extern_crate_name_to_cnum.contains_key(&symbol)
    }
}

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
                    let reexport = {
                        // Find first "opaque" re-export in the chain, which may alter the visible name of the item.
                        let top_level_opaque_reexport = child.reexport_chain.iter()
                            .filter(|reexport| {
                                match reexport {
                                    Reexport::Single(_) => true,
                                    Reexport::Glob(_) => false,
                                    Reexport::ExternCrate(_) => true,
                                    Reexport::MacroUse | Reexport::MacroExport => true,
                                }
                            })
                            .next();

                        top_level_opaque_reexport.copied()
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
                tcx.inherent_impls(def_id).iter()
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

pub fn def_hir_path<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::LocalDefId) -> Vec<(hir::HirId, hir::Node<'tcx>)> {
    let def_hir_id = tcx.local_def_id_to_hir_id(def_id);

    let mut path = tcx.hir_parent_iter(def_hir_id).collect::<Vec<_>>();
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

        | hir::ExprKind::MethodCall(_, _, _, _)
        // NOTE: In addition to explicit function calls and method calls, certain operators
        //       may also result in implicit calls to corresponding trait functions.
        | hir::ExprKind::Unary(_, _)
        | hir::ExprKind::Binary(_, _, _)
        | hir::ExprKind::AssignOp(_, _, _)
        | hir::ExprKind::Index(_, _, _)
        => {
            let Some(def_id) = typeck.type_dependent_def_id(expr.hir_id) else { return None; };
            let generic_args = typeck.node_args(expr.hir_id);
            Some((def_id, generic_args))
        }

        _ => None,
    }
}

struct CalleeCollector<'tcx> {
    typeck: &'tcx ty::TypeckResults<'tcx>,
    current_scope_safety: hir::Safety,
    callees: Vec<Call<'tcx>>,
}

impl<'tcx> hir::intravisit::Visitor<'tcx> for CalleeCollector<'tcx> {
    fn visit_block(&mut self, block: &'tcx hir::Block<'tcx>) {
        let previous_scope_safety = self.current_scope_safety;
        self.current_scope_safety = match block.rules {
            hir::BlockCheckMode::DefaultBlock => self.current_scope_safety,
            hir::BlockCheckMode::UnsafeBlock(_) => hir::Safety::Unsafe,
        };

        hir::intravisit::walk_block(self, block);

        self.current_scope_safety = previous_scope_safety;
    }

    fn visit_expr(&mut self, expr: &'tcx hir::Expr<'tcx>) {
        if let Some((def_id, generic_args)) = callee(self.typeck, expr) {
            self.callees.push(Call {
                kind: CallKind::Def(def_id, generic_args),
                safety: self.current_scope_safety,
                span: expr.span,
            });
        }

        hir::intravisit::walk_expr(self, expr);
    }
}

pub fn collect_callees<'tcx>(tcx: TyCtxt<'tcx>, body: &'tcx hir::Body<'tcx>) -> Vec<Call<'tcx>> {
    let typeck = tcx.typeck_body(body.id());

    fn header_safety(header_safety: hir::HeaderSafety) -> hir::Safety {
        match header_safety {
            hir::HeaderSafety::Normal(safety) => safety,
            hir::HeaderSafety::SafeTargetFeatures => hir::Safety::Unsafe,
        }
    }

    let body_owner = tcx.hir_node(tcx.hir_body_owner(body.id()));
    let body_safety = match body_owner {
        hir::Node::Item(item) => {
            match &item.kind {
                hir::ItemKind::Fn { sig, .. } => header_safety(sig.header.safety),
                _ => hir::Safety::Safe,
            }
        }
        hir::Node::ForeignItem(item) => {
            match &item.kind {
                hir::ForeignItemKind::Fn(_, _, _) => hir::Safety::Unsafe,
                _ => hir::Safety::Safe,
            }
        }
        hir::Node::TraitItem(item) => {
            match &item.kind {
                hir::TraitItemKind::Fn(sig, _) => header_safety(sig.header.safety),
                _ => hir::Safety::Safe,
            }
        }
        hir::Node::ImplItem(item) => {
            match &item.kind {
                hir::ImplItemKind::Fn(sig, _) => header_safety(sig.header.safety),
                _ => hir::Safety::Safe,
            }
        }
        _ => hir::Safety::Safe,
    };

    let mut collector = CalleeCollector {
        typeck,
        current_scope_safety: body_safety,
        callees: vec![],
    };
    collector.visit_body(body);

    collector.callees
}

#[derive(Clone, Debug)]
pub enum DefPathRootKind<'tcx> {
    Global(hir::CrateNum),
    Ty(Ty<'tcx>),
    Local,
    Parent { supers: usize },
}

#[derive(Clone, Debug)]
pub struct DefPathSegment {
    pub def_id: hir::DefId,
    pub ident: Ident,
    pub reexport: Option<Reexport>,
}

#[derive(Clone, Debug)]
pub struct DefPath<'tcx> {
    pub root: DefPathRootKind<'tcx>,
    pub segments: Vec<DefPathSegment>,
}

impl<'tcx> DefPath<'tcx> {
    pub fn new(root: DefPathRootKind<'tcx>, segments: Vec<DefPathSegment>) -> Self {
        Self { root, segments }
    }

    fn from_def_parent_path(tcx: TyCtxt<'tcx>, def_id: hir::DefId) -> Option<Self> {
        let [crate_def_id, def_ids @ ..] = &def_id_path(tcx, def_id)[..] else { unreachable!("empty def id path") };

        let segments = def_ids.into_iter()
            .map(|&def_id| {
                let span = tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
                let name = tcx.opt_item_name(def_id)?;
                Some(DefPathSegment { def_id, ident: Ident::new(name, span), reexport: None })
            })
            .try_collect::<Vec<_>>()?;

        let Some(cnum) = crate_def_id.as_crate_root() else { unreachable!("def id path does not have crate root"); };

        Some(Self::new(DefPathRootKind::Global(cnum), segments))
    }

    pub fn def_id_path(&self) -> impl Iterator<Item = hir::DefId> + '_ {
        self.segments.iter().map(|segment| segment.def_id)
    }

    pub fn ast_path(&self, crate_res: &CrateResolutions<'tcx>, ast_ty_printer: &mut ty::print::AstTyPrinter<'tcx, '_>) -> (Option<P<ast::QSelf>>, ast::Path) {
        use ty::print::Printer;

        let mut segments = self.segments.iter().map(|segment| {
            let ident = segment.ident;
            ast::PathSegment { id: ast::DUMMY_NODE_ID, ident, args: None }
        }).collect::<ThinVec<_>>();

        let mut qself = None;
        match &self.root {
            // `crate::..` paths.
            &DefPathRootKind::Global(cnum) if cnum == LOCAL_CRATE => {
                segments.insert(0, ast::PathSegment { id: ast::DUMMY_NODE_ID, ident: Ident::new(kw::Crate, DUMMY_SP), args: None });
            }
            // `::<crate>::..` paths.
            &DefPathRootKind::Global(cnum) => {
                let crate_name = crate_res.visible_crate_name(cnum);
                segments.splice(0..0, [
                    ast::PathSegment::path_root(DUMMY_SP),
                    ast::PathSegment { id: ast::DUMMY_NODE_ID, ident: Ident::new(crate_name, DUMMY_SP), args: None },
                ]);
            }

            // `<ty>::..` paths to inherent impl assoc items.
            &DefPathRootKind::Ty(ty) => {
                let Ok(ty_ast) = ast_ty_printer.print_ty(ty) else {
                    // FIXME: Give proper diagnostic span for errors.
                    span_bug!(DUMMY_SP, "cannot construct AST representation of type `{ty:?}`");
                };
                qself = Some(P(ast::QSelf { ty: ty_ast, position: 0, path_span: DUMMY_SP }));
            }

            // Local path, no special prefix.
            DefPathRootKind::Local => {}

            // `super::..` paths.
            DefPathRootKind::Parent { supers } => {
                segments.splice(0..0, (0..*supers).map(|_| ast::PathSegment { id: ast::DUMMY_NODE_ID, ident: Ident::new(kw::Super, DUMMY_SP), args: None }));
            }
        }

        (qself, ast::Path { span: DUMMY_SP, segments, tokens: None })
    }
}

pub fn relative_def_path<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, scope: hir::DefId) -> Option<DefPath<'tcx>> {
    if !tcx.is_descendant_of(def_id, scope) { return None; }

    if def_id == scope {
        if let Some(cnum) = def_id.as_crate_root() {
            return Some(DefPath::new(DefPathRootKind::Global(cnum), vec![]));
        }
        let span = tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
        let name = tcx.opt_item_name(def_id)?;
        return Some(DefPath::new(DefPathRootKind::Local, vec![DefPathSegment { def_id, ident: Ident::new(name, span), reexport: None }]));
    }

    let full_def_id_path = def_id_path(tcx, def_id);
    let scope_def_id_path = def_id_path(tcx, scope);
    let mut relative_def_id_path = &full_def_id_path[scope_def_id_path.len()..];

    // NOTE: If we find an inherent impl parent in the relative path,
    //       we modify the path to be type-relative to the type the inherent impl is for.
    //       This technically makes it a "global" (i.e. non-relative) path.
    let mut root = match relative_def_id_path {
        [crate_def_id, ..] if let Some(cnum) = crate_def_id.as_crate_root() => DefPathRootKind::Global(cnum),
        _ => DefPathRootKind::Local,
    };
    for (i, &def_id) in relative_def_id_path.iter().enumerate().rev() {
        let hir::DefKind::Impl { of_trait: false } = tcx.def_kind(def_id) else { continue; };
        let ty::ImplSubject::Inherent(implementer_ty) = tcx.impl_subject(def_id).instantiate_identity() else { unreachable!("encountered trait impl in def path") };

        relative_def_id_path = &relative_def_id_path[(i + 1)..];
        root = DefPathRootKind::Ty(implementer_ty);
        break;
    }

    let mut def_path = DefPath::new(root, Vec::with_capacity(relative_def_id_path.len()));
    for &def_id in relative_def_id_path {
        let span = tcx.def_ident_span(def_id).unwrap_or(DUMMY_SP);
        let name = tcx.opt_item_name(def_id).unwrap_or(sym::empty);
        def_path.segments.push(DefPathSegment { def_id, ident: Ident::new(name, span), reexport: None });
    }

    Some(def_path)
}

pub fn locally_visible_def_path<'tcx>(tcx: TyCtxt<'tcx>, def_id: hir::DefId, mut scope: hir::DefId) -> Result<DefPath<'tcx>, hir::DefId> {
    if !tcx.is_descendant_of(def_id, scope) {
        'fail: {
            // For some scopes, we can make an adjustment and try to find a relative path from the parent scope.
            let is_transparent = |def_kind: hir::DefKind| matches!(def_kind,
                | hir::DefKind::Struct | hir::DefKind::Enum | hir::DefKind::Union | hir::DefKind::Variant | hir::DefKind::TyAlias
                | hir::DefKind::ForeignMod | hir::DefKind::ForeignTy
                | hir::DefKind::Trait | hir::DefKind::Impl { .. } | hir::DefKind::TraitAlias
                | hir::DefKind::Fn | hir::DefKind::Const | hir::DefKind::Static { .. } | hir::DefKind::Ctor(..)
                | hir::DefKind::AssocTy | hir::DefKind::AssocFn | hir::DefKind::AssocConst
                | hir::DefKind::AnonConst | hir::DefKind::InlineConst
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

pub fn visible_def_paths<'tcx>(tcx: TyCtxt<'tcx>, crate_res: &CrateResolutions<'tcx>, def_id: hir::DefId, scope: Option<hir::DefId>, ignore_reexport: Option<hir::DefId>, span: Span, limit: Option<NonZeroUsize>) -> SmallVec<[DefPath<'tcx>; 1]> {
    let mut impl_parents = parent_iter(tcx, def_id).enumerate().filter(|(_, def_id)| matches!(tcx.def_kind(def_id), hir::DefKind::Impl { of_trait: _ }));
    match impl_parents.next() {
        // `..::{impl#?}::$assoc_item::..` path.
        // NOTE: Such paths will never be accessible outside of the scope of the assoc item.
        Some((1.., _)) => { return smallvec![]; }

        // `..::{impl#?}::$assoc_item` path.
        Some((0, impl_parent_def_id)) => {
            let impl_subject = tcx.impl_subject(impl_parent_def_id);
            let ty::ImplSubject::Inherent(implementer_ty) = impl_subject.instantiate_identity() else { unreachable!("encountered trait impl in def path") };

            let ident = tcx.opt_item_ident(def_id).unwrap();
            let def_path = DefPath::new(DefPathRootKind::Ty(implementer_ty), vec![DefPathSegment { def_id, ident, reexport: None }]);

            return smallvec![def_path];
        }

        None => {}
    }

    let extern_crates = tcx.hir_crate_items(()).definitions()
        .filter(|&def_id| matches!(tcx.def_kind(def_id), hir::DefKind::ExternCrate))
        .filter_map(|def_id| {
            let Some(cnum) = tcx.extern_mod_stmt_cnum(def_id) else { return None; };
            Some((def_id, cnum))
        });

    let (root_def_id, root_def_path) = match def_id.as_local() {
        Some(_) => {
            let root_def_id = CRATE_DEF_ID.to_def_id();
            let root_def_path = DefPath::new(DefPathRootKind::Global(LOCAL_CRATE), vec![]);
            (root_def_id, root_def_path)
        }
        None if let crate_name = crate_res.visible_crate_name(def_id.krate) && crate_res.is_in_extern_prelude(crate_name) => {
            let root_def_id = def_id.krate.as_def_id();
            let root_def_path = DefPath::new(DefPathRootKind::Global(def_id.krate), vec![]);
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
                        let crate_name = crate_res.visible_crate_name(cnum);
                        if !crate_res.is_in_extern_prelude(crate_name) { continue; }

                        let root_def_path = DefPath::new(DefPathRootKind::Global(cnum), vec![]);
                        break 'root (visible_def_id, root_def_path);
                    }
                }

                span_bug!(span, "expected `{}` to be reached through another crate in the extern prelude", tcx.def_path_str(def_id));
            };

            let root_def_id = def_id.krate.as_def_id();
            let Some(mut root_def_path) = DefPath::from_def_parent_path(tcx, reachable_extern_crate_def_id.to_def_id()) else { return smallvec![] };

            if let Some(scope) = scope && tcx.is_descendant_of(reachable_extern_crate_def_id.to_def_id(), scope) {
                let scope_def_id_path = def_id_path(tcx, scope);
                root_def_path.segments.splice(0..(scope_def_id_path.len() - 1), []);
                root_def_path.root = DefPathRootKind::Local;
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

                    if let Some(limit) = limit && paths.len() >= limit.get() {
                        return paths;
                    }
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

pub fn visible_def_path<'tcx>(tcx: TyCtxt<'tcx>, crate_res: &CrateResolutions<'tcx>, def_id: hir::DefId, scope: Option<hir::DefId>, ignore_reexport: Option<hir::DefId>, span: Span) -> Option<DefPath<'tcx>> {
    visible_def_paths(tcx, crate_res, def_id, scope, ignore_reexport, span, NonZeroUsize::new(1)).into_iter().next()
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
