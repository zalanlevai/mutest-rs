use std::hash::{Hash, Hasher};

use rustc_ast_lowering::ResolverAstLowering;
use rustc_expand::base::ResolverExpand;
use rustc_hash::{FxHashSet, FxHashMap};
use rustc_middle::hir::nested_filter::OnlyBodies;
use rustc_resolve::Resolver;
use rustc_session::Session;
use smallvec::SmallVec;

use crate::analysis::ast_lowering::{self, AstDefItem};
use crate::analysis::diagnostic::{self, SessionRcSourceMap};
use crate::analysis::hir;
use crate::analysis::res;
use crate::analysis::tests::Test;
use crate::analysis::ty::{self, TyCtxt};
use crate::codegen::ast::{self, P};
use crate::codegen::ast::visit::Visitor;
use crate::codegen::attr;
use crate::codegen::substitution::conflicting_substs;
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, sym};
use crate::codegen::symbols::hygiene::AstPass;

#[derive(Clone)]
pub struct Lowered<A, H> {
    pub ast: A,
    pub hir: H,
}

pub type LoweredFn<'ast, 'hir> = Lowered<ast::InlinedFn<'ast>, hir::InlinedFn<'hir>>;
pub type OwnedLoweredFn<'hir> = Lowered<ast::OwnedInlinedFn, hir::InlinedFn<'hir>>;
pub type LoweredParam<'ast, 'hir> = Lowered<&'ast ast::Param, &'hir hir::Param<'hir>>;
pub type OwnedLoweredParam<'hir> = Lowered<ast::Param, &'hir hir::Param<'hir>>;
pub type LoweredStmt<'ast, 'hir> = Lowered<&'ast ast::Stmt, &'hir hir::Stmt<'hir>>;
pub type OwnedLoweredStmt<'hir> = Lowered<ast::Stmt, &'hir hir::Stmt<'hir>>;
pub type LoweredExpr<'ast, 'hir> = Lowered<&'ast ast::Expr, &'hir hir::Expr<'hir>>;
pub type OwnedLoweredExpr<'hir> = Lowered<ast::Expr, &'hir hir::Expr<'hir>>;

pub enum MutLoc<'ast, 'hir> {
    Fn(LoweredFn<'ast, 'hir>),
    FnParam(LoweredParam<'ast, 'hir>, LoweredFn<'ast, 'hir>),
    FnBodyStmt(LoweredStmt<'ast, 'hir>, LoweredFn<'ast, 'hir>),
    FnBodyExpr(LoweredExpr<'ast, 'hir>, LoweredFn<'ast, 'hir>),
}

pub enum OwnedMutLoc<'hir> {
    Fn(OwnedLoweredFn<'hir>),
    FnParam(OwnedLoweredParam<'hir>, OwnedLoweredFn<'hir>),
    FnBodyStmt(OwnedLoweredStmt<'hir>, OwnedLoweredFn<'hir>),
    FnBodyExpr(OwnedLoweredExpr<'hir>, OwnedLoweredFn<'hir>),
}

impl<'ast, 'hir> MutLoc<'ast, 'hir> {
    pub fn into_owned(&self) -> OwnedMutLoc<'hir> {
        match self {
            Self::Fn(Lowered { ast: fn_ast, hir: fn_hir }) => {
                OwnedMutLoc::Fn(Lowered { ast: fn_ast.into_owned(), hir: *fn_hir })
            }
            Self::FnParam(Lowered { ast: param_ast, hir: param_hir }, Lowered { ast: fn_ast, hir: fn_hir }) => {
                OwnedMutLoc::FnParam(Lowered { ast: (*param_ast).clone(), hir: param_hir }, Lowered { ast: fn_ast.into_owned(), hir: *fn_hir })
            }
            Self::FnBodyStmt(Lowered { ast: stmt_ast, hir: stmt_hir }, Lowered { ast: fn_ast, hir: fn_hir }) => {
                OwnedMutLoc::FnBodyStmt(Lowered { ast: (*stmt_ast).clone(), hir: stmt_hir }, Lowered { ast: fn_ast.into_owned(), hir: *fn_hir })
            }
            Self::FnBodyExpr(Lowered { ast: expr_ast, hir: expr_hir }, Lowered { ast: fn_ast, hir: fn_hir }) => {
                OwnedMutLoc::FnBodyExpr(Lowered { ast: (*expr_ast).clone(), hir: expr_hir }, Lowered { ast: fn_ast.into_owned(), hir: *fn_hir })
            }
        }
    }
}

impl<'hir> OwnedMutLoc<'hir> {
    pub fn span(&self) -> Span {
        match self {
            Self::Fn(lowered_fn) => lowered_fn.ast.span,
            Self::FnParam(lowered_param, _) => lowered_param.ast.span,
            Self::FnBodyStmt(lowered_stmt, _) => lowered_stmt.ast.span,
            Self::FnBodyExpr(lowered_expr, _) => lowered_expr.ast.span,
        }
    }

    pub fn containing_fn(&self) -> Option<&OwnedLoweredFn> {
        match self {
            Self::Fn(lowered_fn) => Some(lowered_fn),
            Self::FnParam(_, lowered_fn) => Some(lowered_fn),
            Self::FnBodyStmt(_, lowered_fn) => Some(lowered_fn),
            Self::FnBodyExpr(_, lowered_fn) => Some(lowered_fn),
        }
    }
}

pub struct MutCtxt<'ast, 'tcx, 'r, 'op> {
    pub tcx: TyCtxt<'tcx>,
    pub resolver: &'op Resolver<'r>,
    pub def_site: Span,
    pub location: MutLoc<'ast, 'tcx>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum SubstLoc {
    InsertBefore(ast::NodeId),
    InsertAfter(ast::NodeId),
    Replace(ast::NodeId),
}

pub enum Subst {
    AstExpr(ast::Expr),
    AstStmt(ast::Stmt),
    AstLocal(Ident, ast::Mutability, Option<P<ast::Ty>>, P<ast::Expr>, Option<P<ast::Expr>>),
}

pub struct SubstDef {
    pub location: SubstLoc,
    pub substitute: Subst,
}

impl SubstDef {
    pub fn new(location: SubstLoc, substitute: Subst) -> Self {
        Self { location, substitute }
    }
}

pub trait Mutation {
    fn display_name(&self) -> String;

    fn span_label(&self) -> String {
        self.display_name()
    }
}

pub trait Operator<'a>: Send + Sync {
    type Mutation: Mutation + 'a;

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)>;
}

pub trait OperatorBoxed<'a>: Send + Sync {
    type Mutation: Mutation + ?Sized + 'a;

    fn try_apply_boxed(&self, mcx: &MutCtxt) -> Option<(Box<Self::Mutation>, SmallVec<[SubstDef; 1]>)>;
}

impl<'a, T: Operator<'a>> OperatorBoxed<'a> for T {
    type Mutation = dyn Mutation + 'a;

    fn try_apply_boxed(&self, mcx: &MutCtxt) -> Option<(Box<Self::Mutation>, SmallVec<[SubstDef; 1]>)>  {
        match self.try_apply(mcx) {
            Some((mutation, substs)) => Some((Box::new(mutation), substs)),
            None => None,
        }
    }
}

pub type Operators<'op, 'm> = &'op [&'op dyn OperatorBoxed<'m, Mutation = dyn Mutation + 'm>];
pub type BoxedMutation<'m> = Box<dyn Mutation + 'm>;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct MutId(u32);

impl MutId {
    pub fn index(&self) -> u32 {
        self.0
    }

    pub fn into_symbol_name(&self) -> String {
        format!("mut_{}", self.0)
    }

    pub fn into_symbol(&self) -> Symbol {
        Symbol::intern(&self.into_symbol_name())
    }
}

pub struct Mut<'hir, 'trg, 'm> {
    pub id: MutId,
    pub target: &'trg Target<'trg>,
    pub location: OwnedMutLoc<'hir>,
    pub is_in_unsafe_block: bool,
    pub mutation: BoxedMutation<'m>,
    pub substs: SmallVec<[SubstDef; 1]>,
}

impl<'hir, 'trg, 'm> Mut<'hir, 'trg, 'm> {
    pub fn display_name(&self) -> String {
        self.mutation.display_name()
    }

    pub fn display_location(&self, sess: &Session) -> String {
        sess.source_map().span_to_embeddable_string(self.location.span())
    }

    pub fn undetected_diagnostic(&self, sess: &Session) -> String {
        let mut diagnostic = sess.struct_span_warn(self.location.span(), "the following mutation was not detected");
        diagnostic.span_label(self.location.span(), &self.mutation.span_label());
        diagnostic::emit_str(diagnostic, sess.rc_source_map())
    }

    pub fn is_unsafe(&self, unsafe_targeting: UnsafeTargeting) -> bool {
        self.is_in_unsafe_block || self.target.is_unsafe(unsafe_targeting)
    }
}

impl<'hir, 'trg, 'm> Eq for Mut<'hir, 'trg, 'm> {}
impl<'hir, 'trg, 'm> PartialEq for Mut<'hir, 'trg, 'm> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'hir, 'trg, 'm> Hash for Mut<'hir, 'trg, 'm> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

/// | Flag       | UnsafeTargeting | `unsafe fn` | `unsafe {}` | `{ unsafe {} }` | `{}` |
/// | ---------- | --------------- | ----------- | ----------- | --------------- | ---- |
/// | --safe     | None            |             |             |                 | M    |
/// | (default)  | Context         |             |             | M               | M    |
/// | --cautious | UnsafeContext   |             |             | unsafe M        | M    |
/// | --unsafe   | Block           | unsafe M    | unsafe M    | M               | M    |
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnsafeTargeting {
    None,
    Context,
    UnsafeContext,
    Block,
}

impl UnsafeTargeting {
    pub fn inner(&self) -> bool {
        matches!(self, Self::Block)
    }

    pub fn outer(&self) -> bool {
        matches!(self, Self::Block | Self::Context | Self::UnsafeContext)
    }
}

struct MutationCollector<'ast, 'tcx, 'r, 'op, 'trg, 'm> {
    pub operators: Operators<'op, 'm>,
    pub tcx: TyCtxt<'tcx>,
    pub resolver: &'op Resolver<'r>,
    pub def_site: Span,
    pub unsafe_targeting: UnsafeTargeting,
    pub target: Option<&'trg Target<'trg>>,
    pub current_fn: Option<LoweredFn<'ast, 'tcx>>,
    pub is_in_unsafe_block: bool,
    pub next_mut_index: u32,
    pub mutations: Vec<Mut<'tcx, 'trg, 'm>>,
}

/// Macro used during mutation collection to apply every mutation operator using the given mutation
/// context.
///
/// This is a macro because the equivalent associated function would require a mutable borrow of
/// the entire `self` struct, which would disallow certain patterns used in the AST visitor.
macro register_mutations($self:ident, $($mcx:tt)+) {
    {
        let mcx = $($mcx)+;

        for operator in $self.operators {
            if let Some((mutation, substs)) = operator.try_apply_boxed(&mcx) {
                $self.mutations.push(Mut {
                    id: MutId($self.next_mut_index),
                    target: $self.target.expect("attempted to collect mutations without a target"),
                    location: mcx.location.into_owned(),
                    is_in_unsafe_block: $self.is_in_unsafe_block,
                    mutation,
                    substs,
                });

                $self.next_mut_index += 1;
            }
        }
    }
}

impl<'ast, 'tcx, 'r, 'op, 'trg, 'm> ast::visit::Visitor<'ast> for MutationCollector<'ast, 'tcx, 'r, 'op, 'trg, 'm> {
    fn visit_fn(&mut self, kind: ast::visit::FnKind<'ast>, span: Span, id: ast::NodeId) {
        let ast::visit::FnKind::Fn(ref ctx, ref ident, sig, vis, generics, body) = kind else { return; };

        let fn_ast = ast::InlinedFn { id, span, ctx: *ctx, ident: *ident, vis, generics, sig, body };

        let fn_hir = match self.tcx.hir().get_by_def_id(self.resolver.local_def_id(fn_ast.id)) {
            hir::Node::Item(&hir::Item { def_id, span, vis_span, ident, ref kind }) => {
                let hir::ItemKind::Fn(sig, generics, body) = kind else { unreachable!(); };
                let body = self.tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::ItemFn(ident, generics, sig.header);
                hir::InlinedFn { def_id, span, ident, kind: fn_kind, vis_span: Some(vis_span), sig, generics, body }
            }
            hir::Node::TraitItem(&hir::TraitItem { def_id, span, ident, ref generics, ref kind }) => {
                let hir::TraitItemKind::Fn(sig, hir::TraitFn::Provided(body)) = kind else { unreachable!(); };
                let body = self.tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::Method(ident, sig);
                hir::InlinedFn { def_id, span, ident, kind: fn_kind, vis_span: None, sig, generics, body }
            }
            hir::Node::ImplItem(&hir::ImplItem { def_id, span, vis_span, ident, ref generics, ref kind }) => {
                let hir::ImplItemKind::Fn(sig, body) = kind else { unreachable!(); };
                let body = self.tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::Method(ident, sig);
                hir::InlinedFn { def_id, span, ident, kind: fn_kind, vis_span: Some(vis_span), sig, generics, body }
            }
            _ => unreachable!(),
        };

        let lowered_fn = Lowered { ast: fn_ast, hir: fn_hir };

        register_mutations!(self, MutCtxt {
            tcx: self.tcx,
            resolver: self.resolver,
            def_site: self.def_site,
            location: MutLoc::Fn(lowered_fn.clone()),
        });

        let kind_ast = kind;
        let span_ast = lowered_fn.ast.span;
        let id_ast = lowered_fn.ast.id;
        let kind_hir = lowered_fn.hir.kind;
        let decl_hir = lowered_fn.hir.sig.decl;
        let body_hir = lowered_fn.hir.body.id();
        let span_hir = lowered_fn.hir.span;
        let id_hir = self.tcx.hir().local_def_id_to_hir_id(lowered_fn.hir.def_id);
        self.current_fn = Some(lowered_fn);
        ast_lowering::visit::AstHirVisitor::visit_fn(self, kind_ast, span_ast, id_ast, kind_hir, decl_hir, body_hir, span_hir, id_hir);
        self.current_fn = None;
    }
}

impl<'ast, 'hir, 'r, 'op, 'trg, 'm> ast_lowering::visit::AstHirVisitor<'ast, 'hir> for MutationCollector<'ast, 'hir, 'r, 'op, 'trg, 'm> {
    type NestedFilter = OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_param(&mut self, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>) {
        if !self.tcx.sess.source_map().is_local_span(param_ast.span) { return; };
        if attr::ignore(self.tcx.hir().attrs(param_hir.hir_id)) { return; }

        if let Some(lowered_fn) = &self.current_fn {
            let lowered_param = Lowered { ast: param_ast, hir: param_hir };

            register_mutations!(self, MutCtxt {
                tcx: self.tcx,
                resolver: self.resolver,
                def_site: self.def_site,
                location: MutLoc::FnParam(lowered_param, lowered_fn.clone()),
            });
        }

        ast_lowering::visit::walk_param(self, param_ast, param_hir);
    }

    fn visit_block(&mut self, block_ast: &'ast ast::Block, block_hir: &'hir hir::Block<'hir>) {
        if !self.tcx.sess.source_map().is_local_span(block_ast.span) { return; };
        if attr::ignore(self.tcx.hir().attrs(block_hir.hir_id)) { return; }
        if !self.unsafe_targeting.inner() && let ast::BlockCheckMode::Unsafe(_) = block_ast.rules { return; }

        let is_in_unsafe_block = self.is_in_unsafe_block;
        if let ast::BlockCheckMode::Unsafe(_) = block_ast.rules { self.is_in_unsafe_block = true; }
        ast_lowering::visit::walk_block(self, block_ast, block_hir);
        if let ast::BlockCheckMode::Unsafe(_) = block_ast.rules { self.is_in_unsafe_block = is_in_unsafe_block; }
    }

    fn visit_stmt(&mut self, stmt_ast: &'ast ast::Stmt, stmt_hir: &'hir hir::Stmt<'hir>) {
        if !self.tcx.sess.source_map().is_local_span(stmt_ast.span) { return; };
        if attr::ignore(self.tcx.hir().attrs(stmt_hir.hir_id)) { return; }

        if let Some(lowered_fn) = &self.current_fn {
            let lowered_stmt = Lowered { ast: stmt_ast, hir: stmt_hir };

            register_mutations!(self, MutCtxt {
                tcx: self.tcx,
                resolver: self.resolver,
                def_site: self.def_site,
                location: MutLoc::FnBodyStmt(lowered_stmt, lowered_fn.clone()),
            });
        }

        ast_lowering::visit::walk_stmt(self, stmt_ast, stmt_hir);
    }

    fn visit_expr(&mut self, expr_ast: &'ast ast::Expr, expr_hir: &'hir hir::Expr<'hir>) {
        if !self.tcx.sess.source_map().is_local_span(expr_ast.span) { return; };
        if attr::ignore(self.tcx.hir().attrs(expr_hir.hir_id)) { return; }

        if let Some(lowered_fn) = &self.current_fn {
            let lowered_expr = Lowered { ast: expr_ast, hir: expr_hir };

            register_mutations!(self, MutCtxt {
                tcx: self.tcx,
                resolver: self.resolver,
                def_site: self.def_site,
                location: MutLoc::FnBodyExpr(lowered_expr, lowered_fn.clone()),
            });
        }

        ast_lowering::visit::walk_expr(self, expr_ast, expr_hir);
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Unsafety {
    None,
    Decl,
    Body,
}

impl Unsafety {
    pub fn inner(&self) -> bool {
        matches!(self, Self::Body)
    }

    pub fn any(&self) -> bool {
        matches!(self, Self::Decl | Self::Body)
    }
}

struct BodyUnsafetyChecker {
    unsafety: Option<Unsafety>,
}

impl<'ast> ast::visit::Visitor<'ast> for BodyUnsafetyChecker {
    fn visit_block(&mut self, block: &'ast ast::Block) {
        if let ast::BlockCheckMode::Unsafe(ast::UnsafeSource::UserProvided) = block.rules {
            self.unsafety = Some(Unsafety::Body);
            return;
        }

        ast::visit::walk_block(self, block);
    }
}

fn check_target_unsafety<'ast>(item: AstDefItem<'ast>) -> Unsafety {
    let ast::ItemKind::Fn(target_fn) = item.kind() else { return Unsafety::None };

    let ast::Unsafe::No = target_fn.sig.header.unsafety else { return Unsafety::Decl };

    let Some(target_body) = target_fn.body else { return Unsafety::None };
    let mut checker = BodyUnsafetyChecker { unsafety: None };
    checker.visit_block(&target_body);
    checker.unsafety.unwrap_or(Unsafety::None)
}

pub struct Target<'tst> {
    pub def_id: hir::LocalDefId,
    pub unsafety: Unsafety,
    pub reachable_from: FxHashMap<&'tst Test, usize>,
    pub distance: usize,
}

impl<'tst> Target<'tst> {
    pub fn is_unsafe(&self, unsafe_targeting: UnsafeTargeting) -> bool {
        matches!((unsafe_targeting, self.unsafety),
            | (_, Unsafety::Decl)
            | (UnsafeTargeting::None, Unsafety::Body)
            | (UnsafeTargeting::UnsafeContext, Unsafety::Body)
        )
    }
}

pub fn reachable_fns<'ast, 'tcx, 'tst>(tcx: TyCtxt<'tcx>, resolver: &mut Resolver, krate: &'ast ast::Crate, tests: &'tst [Test], depth: usize) -> Vec<Target<'tst>> {
    let mut previously_found_callees: FxHashMap<(hir::LocalDefId, Option<ty::SubstsRef<'tcx>>), Vec<&'tst Test>> = Default::default();

    for test in tests {
        let Some(def_id) = resolver.opt_local_def_id(test.item.id) else { continue; };
        let body = tcx.hir().body(tcx.hir().get_by_def_id(def_id).body_id().unwrap());

        let mut callees = FxHashSet::from_iter(res::collect_callees(tcx, body));

        for (callee, substs) in callees.drain() {
            let Some(callee_def_id) = callee.as_local() else { continue; };

            previously_found_callees.entry((callee_def_id, substs))
                .and_modify(|reachable_from| reachable_from.push(test))
                .or_insert_with(|| vec![test]);
        }
    }

    let mut targets: FxHashMap<hir::LocalDefId, Target> = Default::default();

    for distance in 0..depth {
        let mut newly_found_callees: FxHashMap<(hir::LocalDefId, Option<ty::SubstsRef<'tcx>>), Vec<&'tst Test>> = Default::default();

        for ((callee_def_id, outer_substs), reachable_from) in previously_found_callees.drain() {
            let Some(body_id) = tcx.hir().get_by_def_id(callee_def_id).body_id() else { continue; };
            let body = tcx.hir().body(body_id);

            let Some(callee_def_item) = ast_lowering::find_def_in_ast(tcx, callee_def_id, krate) else { continue; };

            if !attr::skip(tcx.hir().attrs(tcx.hir().local_def_id_to_hir_id(callee_def_id))) {
                targets.entry(callee_def_id)
                    .and_modify(|target| {
                        for &test in &reachable_from {
                            target.reachable_from.entry(test).or_insert(distance);
                        }
                    })
                    .or_insert_with(|| Target {
                        def_id: callee_def_id,
                        unsafety: check_target_unsafety(callee_def_item),
                        reachable_from: reachable_from.iter().map(|&test| (test, distance)).collect(),
                        distance,
                    });
            }

            if distance < depth {
                let mut callees = FxHashSet::from_iter(res::collect_callees(tcx, body));

                for (callee, substs) in callees.drain() {
                    let Some(callee_def_id) = callee.as_local() else { continue; };

                    let param_env = tcx.param_env(callee);
                    let instance = substs.or(outer_substs).and_then(|substs| tcx.resolve_instance(param_env.and((callee, substs))).ok().flatten());

                    let callee_def_id = instance.as_ref().map(ty::Instance::def_id).and_then(hir::DefId::as_local).unwrap_or(callee_def_id);

                    newly_found_callees.entry((callee_def_id, substs))
                        .and_modify(|previously_reachable_from| previously_reachable_from.extend(reachable_from.clone()))
                        .or_insert_with(|| reachable_from.clone());
                }
            }
        }

        previously_found_callees.extend(newly_found_callees.drain());
    }

    targets.into_values().collect()
}

pub fn apply_mutation_operators<'ast, 'tcx, 'trg, 'm>(tcx: TyCtxt<'tcx>, resolver: &mut Resolver, krate: &'ast ast::Crate, targets: &'trg [Target<'trg>], ops: Operators<'_, 'm>, unsafe_targeting: UnsafeTargeting) -> Vec<Mut<'tcx, 'trg, 'm>> {
    let expn_id = resolver.expansion_for_ast_pass(
        DUMMY_SP,
        AstPass::TestHarness,
        &[sym::rustc_attrs],
        None,
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let mut collector = MutationCollector {
        operators: ops,
        tcx,
        resolver,
        def_site,
        unsafe_targeting,
        target: None,
        current_fn: None,
        is_in_unsafe_block: false,
        next_mut_index: 1,
        mutations: vec![],
    };

    for target in targets {
        if unsafe_targeting == UnsafeTargeting::None && target.unsafety.any() { continue; }
        if !unsafe_targeting.inner() && target.unsafety == Unsafety::Decl { continue; }

        collector.target = Some(target);
        collector.is_in_unsafe_block = target.unsafety == Unsafety::Decl;

        let Some(target_item) = ast_lowering::find_def_in_ast(tcx, target.def_id, krate) else { continue; };

        match target_item {
            AstDefItem::Item(item) => collector.visit_item(item),
            AstDefItem::ForeignItem(item) => collector.visit_foreign_item(item),
            AstDefItem::AssocItem(item, ctx) => collector.visit_assoc_item(item, ctx),
        }
    }

    collector.mutations
}

pub struct Mutant<'tcx, 'trg, 'm> {
    pub mutations: Vec<Mut<'tcx, 'trg, 'm>>,
}

impl<'tcx, 'trg, 'm> Mutant<'tcx, 'trg, 'm> {
    pub fn iter_mutations(&self) -> impl Iterator<Item = &BoxedMutation<'m>> {
        self.mutations.iter().map(|m| &m.mutation)
    }

    pub fn iter_substitutions(&self) -> impl Iterator<Item = &SubstDef> {
        self.mutations.iter().flat_map(|m| &m.substs)
    }
}

pub fn conflicting_targets(a: &Target, b: &Target) -> bool {
    let reachable_from_a = a.reachable_from.iter().map(|(test, _)| test.item.id).collect();
    let reachable_from_b = b.reachable_from.iter().map(|(test, _)| test.item.id).collect();
    !FxHashSet::is_disjoint(&reachable_from_a, &reachable_from_b)
}

pub fn batch_mutations<'tcx, 'trg, 'm>(mutations: Vec<Mut<'tcx, 'trg, 'm>>, mutant_max_mutations_count: usize, unsafe_targeting: UnsafeTargeting) -> Vec<Mutant<'tcx, 'trg, 'm>> {
    let mut mutants: Vec<Mutant<'tcx, 'trg, 'm>> = vec![];

    'mutation: for mutation in mutations {
        'mutant: for mutant in &mut mutants {
            if mutant.mutations.len() >= mutant_max_mutations_count { continue 'mutant; }

            for subst in &mutation.substs {
                if mutant.iter_substitutions().any(|s| conflicting_substs(s, subst)) {
                    continue 'mutant;
                }
            }

            if mutation.is_unsafe(unsafe_targeting) { break 'mutant; }
            if mutant.mutations.iter().any(|m| m.is_unsafe(unsafe_targeting)) { continue 'mutant; }

            if mutant.mutations.iter().any(|m| conflicting_targets(m.target, mutation.target)) {
                continue 'mutant;
            }

            mutant.mutations.push(mutation);
            continue 'mutation;
        }

        mutants.push(Mutant {
            mutations: vec![mutation],
        });
    }

    mutants
}
