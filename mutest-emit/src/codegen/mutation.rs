use std::hash::{Hash, Hasher};

use rustc_hash::{FxHashSet, FxHashMap};
use rustc_middle::hir::nested_filter::OnlyBodies;
use rustc_session::Session;
use rustc_span::source_map::SourceMap;
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
use crate::codegen::expansion::TcxExpansionExt;
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

pub struct MutCtxt<'ast, 'tcx, 'op> {
    pub tcx: TyCtxt<'tcx>,
    pub resolutions: &'op ast_lowering::Resolutions,
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
        let mut diagnostic = sess.struct_span_warn(self.location.span(), "mutation was not detected");
        diagnostic.span_label(self.location.span(), self.mutation.span_label());
        diagnostic::emit_str(diagnostic, sess.rc_source_map())
    }

    pub fn is_unsafe(&self, unsafe_targeting: UnsafeTargeting) -> bool {
        self.is_in_unsafe_block || self.target.unsafety.is_unsafe(unsafe_targeting)
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

/// | Flag       | UnsafeTargeting       | `unsafe {}` | `{ unsafe {} }` | `{}` |
/// | ---------- | --------------------- | ----------- | --------------- | ---- |
/// | --safe     | None                  |             |                 | M    |
/// | --cautious | OnlyEnclosing(Unsafe) |             | unsafe M        | M    |
/// | (default)  | OnlyEnclosing(Normal) |             | M               | M    |
/// | --unsafe   | All                   | unsafe M    | M               | M    |
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnsafeTargeting {
    None,
    OnlyEnclosing(hir::Unsafety),
    All,
}

impl UnsafeTargeting {
    pub fn any(&self) -> bool {
        !matches!(self, Self::None)
    }

    pub fn inside_unsafe(&self) -> bool {
        matches!(self, Self::All)
    }

    pub fn enclosing_unsafe(&self) -> bool {
        matches!(self, Self::All | Self::OnlyEnclosing(_))
    }
}

struct MutationCollector<'ast, 'tcx, 'op, 'trg, 'm> {
    pub operators: Operators<'op, 'm>,
    pub tcx: TyCtxt<'tcx>,
    pub resolutions: &'op ast_lowering::Resolutions,
    pub def_site: Span,
    pub unsafe_targeting: UnsafeTargeting,
    pub target: Option<&'trg Target<'trg>>,
    pub current_fn: Option<LoweredFn<'ast, 'tcx>>,
    pub current_closure: Option<hir::BodyId>,
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


impl<'ast, 'tcx, 'op, 'trg, 'm> ast::visit::Visitor<'ast> for MutationCollector<'ast, 'tcx, 'op, 'trg, 'm> {
    fn visit_fn(&mut self, kind: ast::visit::FnKind<'ast>, span: Span, id: ast::NodeId) {
        let ast::visit::FnKind::Fn(ref ctx, ref ident, sig, vis, generics, body) = kind else { return; };

        let fn_ast = ast::InlinedFn { id, span, ctx: *ctx, ident: *ident, vis, generics, sig, body };

        let Some(fn_def_id) = self.resolutions.node_id_to_def_id.get(&fn_ast.id).copied() else { unreachable!() };
        let fn_hir = match self.tcx.hir().get_by_def_id(fn_def_id) {
            hir::Node::Item(&hir::Item { owner_id, span, vis_span, ident, ref kind }) => {
                let hir::ItemKind::Fn(sig, generics, body) = kind else { unreachable!(); };
                let body = self.tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::ItemFn(ident, generics, sig.header);
                hir::InlinedFn { def_id: owner_id.def_id, span, ident, kind: fn_kind, vis_span: Some(vis_span), sig, generics, body }
            }
            hir::Node::TraitItem(&hir::TraitItem { owner_id, span, ident, ref generics, ref kind, defaultness }) => {
                let hir::TraitItemKind::Fn(sig, hir::TraitFn::Provided(body)) = kind else { unreachable!(); };
                let body = self.tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::Method(ident, sig);
                hir::InlinedFn { def_id: owner_id.def_id, span, ident, kind: fn_kind, vis_span: None, sig, generics, body }
            }
            hir::Node::ImplItem(&hir::ImplItem { owner_id, span, vis_span, ident, ref generics, ref kind, defaultness }) => {
                let hir::ImplItemKind::Fn(sig, body) = kind else { unreachable!(); };
                let body = self.tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::Method(ident, sig);
                hir::InlinedFn { def_id: owner_id.def_id, span, ident, kind: fn_kind, vis_span: Some(vis_span), sig, generics, body }
            }
            _ => unreachable!(),
        };

        let lowered_fn = Lowered { ast: fn_ast, hir: fn_hir };

        register_mutations!(self, MutCtxt {
            tcx: self.tcx,
            resolutions: self.resolutions,
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

fn is_local_span(source_map: &SourceMap, sp: Span) -> bool {
    let local_begin = source_map.lookup_byte_offset(sp.lo());
    let local_end = source_map.lookup_byte_offset(sp.hi());
    local_begin.sf.src.is_some() && local_end.sf.src.is_some()
}

impl<'ast, 'hir, 'op, 'trg, 'm> ast_lowering::visit::AstHirVisitor<'ast, 'hir> for MutationCollector<'ast, 'hir, 'op, 'trg, 'm> {
    type NestedFilter = OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_param(&mut self, param_ast: &'ast ast::Param, param_hir: &'hir hir::Param<'hir>) {
        if !is_local_span(self.tcx.sess.source_map(), param_ast.span) { return; };
        if attr::ignore(self.tcx.hir().attrs(param_hir.hir_id)) { return; }

        if let Some(lowered_fn) = &self.current_fn {
            // FIXME: Nested function bodies are currently not represented in `MutLoc`, so we skip them for now to
            //        avoid generating leaking, malformed mutations.
            if let Some(_) = self.current_closure { return; }

            let lowered_param = Lowered { ast: param_ast, hir: param_hir };

            register_mutations!(self, MutCtxt {
                tcx: self.tcx,
                resolutions: self.resolutions,
                def_site: self.def_site,
                location: MutLoc::FnParam(lowered_param, lowered_fn.clone()),
            });
        }

        ast_lowering::visit::walk_param(self, param_ast, param_hir);
    }

    fn visit_block(&mut self, block_ast: &'ast ast::Block, block_hir: &'hir hir::Block<'hir>) {
        if !is_local_span(self.tcx.sess.source_map(), block_ast.span) { return; };
        if attr::ignore(self.tcx.hir().attrs(block_hir.hir_id)) { return; }
        if !self.unsafe_targeting.inside_unsafe() && let ast::BlockCheckMode::Unsafe(_) = block_ast.rules { return; }

        let is_in_unsafe_block = self.is_in_unsafe_block;
        if let ast::BlockCheckMode::Unsafe(_) = block_ast.rules { self.is_in_unsafe_block = true; }
        ast_lowering::visit::walk_block(self, block_ast, block_hir);
        if let ast::BlockCheckMode::Unsafe(_) = block_ast.rules { self.is_in_unsafe_block = is_in_unsafe_block; }
    }

    fn visit_stmt(&mut self, stmt_ast: &'ast ast::Stmt, stmt_hir: &'hir hir::Stmt<'hir>) {
        if !is_local_span(self.tcx.sess.source_map(), stmt_ast.span) { return; };
        if attr::ignore(self.tcx.hir().attrs(stmt_hir.hir_id)) { return; }

        if let Some(lowered_fn) = &self.current_fn {
            // FIXME: Nested function bodies are currently not represented in `MutLoc`, so we skip them for now to
            //        avoid generating leaking, malformed mutations.
            if let Some(_) = self.current_closure { return; }

            let lowered_stmt = Lowered { ast: stmt_ast, hir: stmt_hir };

            register_mutations!(self, MutCtxt {
                tcx: self.tcx,
                resolutions: self.resolutions,
                def_site: self.def_site,
                location: MutLoc::FnBodyStmt(lowered_stmt, lowered_fn.clone()),
            });
        }

        ast_lowering::visit::walk_stmt(self, stmt_ast, stmt_hir);
    }

    fn visit_expr(&mut self, expr_ast: &'ast ast::Expr, expr_hir: &'hir hir::Expr<'hir>) {
        if !is_local_span(self.tcx.sess.source_map(), expr_ast.span) { return; };
        if attr::ignore(self.tcx.hir().attrs(expr_hir.hir_id)) { return; }

        if let Some(lowered_fn) = &self.current_fn {
            // FIXME: Nested function bodies are currently not represented in `MutLoc`, so we skip them for now to
            //        avoid generating leaking, malformed mutations.
            if let Some(_) = self.current_closure { return; }

            // Ignore block expressions with only a single nested node, visit the nested node instead.
            if let ast::ExprKind::Block(block_ast, _) = &expr_ast.kind && block_ast.stmts.len() == 1 {
                return ast_lowering::visit::walk_expr(self, expr_ast, expr_hir);
            }

            let lowered_expr = Lowered { ast: expr_ast, hir: expr_hir };

            register_mutations!(self, MutCtxt {
                tcx: self.tcx,
                resolutions: self.resolutions,
                def_site: self.def_site,
                location: MutLoc::FnBodyExpr(lowered_expr, lowered_fn.clone()),
            });
        }

        let current_closure = self.current_closure;
        if let hir::ExprKind::Closure(&hir::Closure { body, ..  }) = expr_hir.kind { self.current_closure = Some(body); }

        match (&expr_ast.kind, &expr_hir.kind) {
            // The left-hand side of assignment expressions only supports a strict subset of expressions, not including
            // the branching match expressions we use for substitutions, so we only mutate the right-hand side.
            (ast::ExprKind::Assign(_lhs_ast, rhs_ast, _), hir::ExprKind::Assign(_lhs_hir, rhs_hir, _)) => {
                ast_lowering::visit::visit_matching_expr(self, rhs_ast, rhs_hir);
            }
            (ast::ExprKind::AssignOp(_, _lhs_ast, rhs_ast), hir::ExprKind::AssignOp(_, _lhs_hir, rhs_hir)) => {
                ast_lowering::visit::visit_matching_expr(self, rhs_ast, rhs_hir);
            }
            // The `else` branch of an `if` conditional must be either another `if` conditional or a block, so we do
            // not mutate `else` blocks directly, instead visiting its contents.
            (ast::ExprKind::If(cond_ast, then_ast, els_ast), hir::ExprKind::If(cond_hir, then_hir, els_hir)) => {
                ast_lowering::visit::visit_matching_expr(self, cond_ast, cond_hir);
                ast_lowering::visit::visit_block_expr(self, then_ast, then_hir);
                if let Some(els_ast) = els_ast && let Some(els_hir) = els_hir {
                    match &els_ast.kind {
                        ast::ExprKind::Block(_, _) => ast_lowering::visit::walk_expr(self, els_ast, els_hir),
                        _ => ast_lowering::visit::visit_matching_expr(self, els_ast, els_hir),
                    }
                }
            }
            _ => ast_lowering::visit::walk_expr(self, expr_ast, expr_hir),
        }

        if let hir::ExprKind::Closure(_) = expr_hir.kind { self.current_closure = current_closure; }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnsafeSource {
    EnclosingUnsafe,
    Unsafe,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Unsafety {
    None,
    /// Safe code called from an unsafe context.
    Tainted(UnsafeSource),
    Unsafe(UnsafeSource),
}

impl Unsafety {
    pub fn any(&self) -> bool {
        !matches!(self, Self::None)
    }

    pub fn is_unsafe(&self, unsafe_targeting: UnsafeTargeting) -> bool {
        matches!((unsafe_targeting, self),
            | (_, Unsafety::Unsafe(UnsafeSource::Unsafe) | Unsafety::Tainted(UnsafeSource::Unsafe))
            | (UnsafeTargeting::None, Unsafety::Unsafe(_) | Unsafety::Tainted(_))
            | (UnsafeTargeting::OnlyEnclosing(hir::Unsafety::Unsafe), Unsafety::Unsafe(UnsafeSource::EnclosingUnsafe) | Unsafety::Tainted(UnsafeSource::EnclosingUnsafe))
        )
    }
}

struct BodyUnsafetyChecker {
    unsafety: Option<Unsafety>,
}

impl<'ast> ast::visit::Visitor<'ast> for BodyUnsafetyChecker {
    fn visit_block(&mut self, block: &'ast ast::Block) {
        if let ast::BlockCheckMode::Unsafe(ast::UnsafeSource::UserProvided) = block.rules {
            self.unsafety = Some(Unsafety::Unsafe(UnsafeSource::EnclosingUnsafe));
            return;
        }

        ast::visit::walk_block(self, block);
    }
}

fn check_item_unsafety<'ast>(item: AstDefItem<'ast>) -> Unsafety {
    let ast::ItemKind::Fn(target_fn) = item.kind() else { return Unsafety::None };

    let ast::Unsafe::No = target_fn.sig.header.unsafety else { return Unsafety::Unsafe(UnsafeSource::Unsafe) };

    let Some(target_body) = target_fn.body else { return Unsafety::None };
    let mut checker = BodyUnsafetyChecker { unsafety: None };
    checker.visit_block(&target_body);
    checker.unsafety.unwrap_or(Unsafety::None)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EntryPointAssociation {
    pub distance: usize,
    pub unsafe_call_path: Option<UnsafeSource>,
}

#[derive(Debug)]
pub struct Target<'tst> {
    pub def_id: hir::LocalDefId,
    pub unsafety: Unsafety,
    pub reachable_from: FxHashMap<&'tst Test, EntryPointAssociation>,
    pub distance: usize,
}

impl<'tst> Target<'tst> {
    pub fn is_tainted(&self, entry_point: &Test, unsafe_targeting: UnsafeTargeting) -> bool {
        self.reachable_from.get(entry_point).is_some_and(|entry_point| {
            let unsafety = entry_point.unsafe_call_path.map(Unsafety::Tainted).unwrap_or(Unsafety::None);
            unsafety.is_unsafe(unsafe_targeting)
        })
    }
}

pub fn reachable_fns<'ast, 'tcx, 'tst>(tcx: TyCtxt<'tcx>, resolutions: &ast_lowering::Resolutions, krate: &'ast ast::Crate, tests: &'tst [Test], depth: usize) -> Vec<Target<'tst>> {
    type Callee<'tcx> = (hir::LocalDefId, ty::GenericArgsRef<'tcx>);

    /// A map from each entry point to the most severe unsafety source of any call path in its current call tree walk.
    /// Safe items called from an unsafe context (dependencies) will be marked `Unsafety::Tainted` with their
    /// corresponding unsafety source.
    ///
    /// ```ignore
    /// [Safe] fn x { [None -> Safe]
    ///     [Safe] fn y { [Some(EnclosingUnsafe) -> Unsafe(EnclosingUnsafe)]
    ///         unsafe { [Some(Unsafe) -> Unsafe(Unsafe)]
    ///             [Safe] fn z { [Some(Unsafe) -> Tainted(Unsafe)] }
    ///         }
    ///         [Safe] fn w { [Some(EnclosingUnsafe) -> Tainted(EnclosingUnsafe)] }
    ///         [Unsafe(Unsafe)] unsafe fn u { [Some(Unsafe) -> Unsafe(Unsafe)]
    ///             [Safe] fn v { [Some(Unsafe) -> Tainted(Unsafe)] }
    ///             [Safe] fn w { [Some(Unsafe) -> Tainted(Unsafe)] }
    ///         }
    ///     }
    /// }
    /// ```
    type CallPaths<'tst> = FxHashMap<&'tst Test, Option<UnsafeSource>>;

    let mut previously_found_callees: FxHashMap<Callee<'tcx>, CallPaths<'tst>> = Default::default();

    for test in tests {
        let Some(def_id) = resolutions.node_id_to_def_id.get(&test.item.id).copied() else { continue; };
        let body = tcx.hir().body(tcx.hir().get_by_def_id(def_id).body_id().unwrap());

        let mut callees = FxHashSet::from_iter(res::collect_callees(tcx, body));

        for call in callees.drain() {
            let Some(local_def_id) = call.def_id.as_local() else { continue; };

            let param_env = tcx.param_env(call.def_id);
            // Using the concrete type arguments of this call, we resolve the corresponding definition instance. The
            // type arguments might take a different form at the resolved definition site, so we propagate them
            // instead.
            let instance = tcx.resolve_instance(param_env.and((call.def_id, call.generic_args))).ok().flatten();
            let (callee_def_id, generic_args) = instance
                .and_then(|instance| instance.def_id().as_local().map(|def_id| (def_id, instance.args)))
                .unwrap_or((local_def_id, call.generic_args));

            let call_paths = previously_found_callees.entry((callee_def_id, generic_args)).or_insert_with(Default::default);
            call_paths.insert(test, None);
        }
    }

    let mut targets: FxHashMap<hir::LocalDefId, Target> = Default::default();

    for distance in 0..depth {
        let mut newly_found_callees: FxHashMap<Callee<'tcx>, CallPaths<'tst>> = Default::default();

        for ((caller_def_id, outer_generic_args), call_paths) in previously_found_callees.drain() {
            let Some(body_id) = tcx.hir().get_by_def_id(caller_def_id).body_id() else { continue; };
            let body = tcx.hir().body(body_id);

            let Some(caller_def_item) = ast_lowering::find_def_in_ast(tcx, caller_def_id, krate) else { continue; };

            if !attr::skip(tcx.hir().attrs(tcx.hir().local_def_id_to_hir_id(caller_def_id))) {
                let target = targets.entry(caller_def_id).or_insert_with(|| {
                    Target {
                        def_id: caller_def_id,
                        unsafety: check_item_unsafety(caller_def_item),
                        reachable_from: Default::default(),
                        distance,
                    }
                });

                for (&test, &unsafety) in &call_paths {
                    let caller_tainting = unsafety.map(Unsafety::Tainted).unwrap_or(Unsafety::None);
                    target.unsafety = Ord::max(caller_tainting, target.unsafety);

                    let entry_point = target.reachable_from.entry(test).or_insert_with(|| {
                        EntryPointAssociation {
                            distance,
                            unsafe_call_path: None,
                        }
                    });

                    entry_point.unsafe_call_path = Ord::max(unsafety, entry_point.unsafe_call_path);
                }
            }

            if distance < depth {
                let mut callees = FxHashSet::from_iter(res::collect_callees(tcx, body));

                for call in callees.drain() {
                    let Some(local_def_id) = call.def_id.as_local() else { continue; };

                    let param_env = tcx.param_env(call.def_id);
                    // The type arguments from the local, generic scope may still contain type parameters, so we
                    // fold the bound type arguments of the concrete invocation of the enclosing function into it.
                    let generic_args = res::instantiate_generic_args(tcx, call.generic_args, outer_generic_args);
                    // Using the concrete type arguments of this call, we resolve the corresponding definition
                    // instance. The type arguments might take a different form at the resolved definition site, so
                    // we propagate them instead.
                    let instance = tcx.resolve_instance(param_env.and((call.def_id, generic_args))).ok().flatten();
                    let (callee_def_id, generic_args) = instance
                        .and_then(|instance| instance.def_id().as_local().map(|def_id| (def_id, instance.args)))
                        .unwrap_or((local_def_id, call.generic_args));

                    let new_call_paths = newly_found_callees.entry((callee_def_id, generic_args)).or_insert_with(Default::default);

                    for (&test, &unsafety) in &call_paths {
                        let unsafe_source = match call.unsafety {
                            hir::Unsafety::Normal => unsafety,
                            hir::Unsafety::Unsafe => Some(UnsafeSource::Unsafe),
                        };

                        let new_unsafety = new_call_paths.entry(test).or_insert(unsafety);
                        *new_unsafety = new_unsafety.or(unsafe_source);
                    }
                }
            }
        }

        previously_found_callees.extend(newly_found_callees.drain());
    }

    targets.into_values().collect()
}

pub fn apply_mutation_operators<'ast, 'tcx, 'r, 'trg, 'm>(tcx: TyCtxt<'tcx>, resolutions: &ast_lowering::Resolutions, krate: &'ast ast::Crate, targets: &'trg [Target<'trg>], ops: Operators<'_, 'm>, unsafe_targeting: UnsafeTargeting) -> Vec<Mut<'tcx, 'trg, 'm>> {
    let expn_id = tcx.expansion_for_ast_pass(
        AstPass::TestHarness,
        DUMMY_SP,
        &[sym::rustc_attrs],
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let mut collector = MutationCollector {
        operators: ops,
        tcx,
        resolutions,
        def_site,
        unsafe_targeting,
        target: None,
        current_fn: None,
        current_closure: None,
        is_in_unsafe_block: false,
        next_mut_index: 1,
        mutations: vec![],
    };

    for target in targets {
        if !unsafe_targeting.any() && target.unsafety.any() { continue; }
        if !unsafe_targeting.inside_unsafe() && let Unsafety::Unsafe(UnsafeSource::Unsafe) | Unsafety::Tainted(UnsafeSource::Unsafe) = target.unsafety { continue; }

        collector.target = Some(target);
        collector.is_in_unsafe_block = target.unsafety == Unsafety::Unsafe(UnsafeSource::Unsafe);

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

pub fn batch_mutations<'tcx, 'trg, 'm>(mut mutations: Vec<Mut<'tcx, 'trg, 'm>>, mutant_max_mutations_count: usize, unsafe_targeting: UnsafeTargeting) -> Vec<Mutant<'tcx, 'trg, 'm>> {
    let conflicting_mutations = {
        let mut mutation_conflicts: FxHashSet<(MutId, MutId)> = Default::default();

        let mut iterator = mutations.iter();
        while let Some(mutation) = iterator.next() {
            for other in iterator.clone() {
                let is_conflicting = false
                    || mutation.is_unsafe(unsafe_targeting)
                    || other.is_unsafe(unsafe_targeting)
                    || conflicting_targets(&mutation.target, &other.target);

                if is_conflicting {
                    mutation_conflicts.insert((mutation.id, other.id));
                }
            }
        }

        move |a: MutId, b: MutId| mutation_conflicts.contains(&(a, b)) || mutation_conflicts.contains(&(b, a))
    };

    let mutation_conflict_heuristic = mutations.iter()
        .map(|mutation| {
            let mut conflict_heuristic = 0_usize;

            for other in &mutations {
                if mutation == other { continue; }

                if conflicting_mutations(mutation.id, other.id) {
                    conflict_heuristic += 1;
                }
            }

            (mutation.id, conflict_heuristic)
        })
        .collect::<FxHashMap<_, _>>();

    mutations.sort_by(|a, b| Ord::cmp(&mutation_conflict_heuristic.get(&a.id), &mutation_conflict_heuristic.get(&b.id)).reverse());

    let mut mutants: Vec<Mutant<'tcx, 'trg, 'm>> = vec![];

    for mutation in mutations {
        let mutant_candidate = 'mutant_candidate: {
            // Unsafe mutations are isolated into their own mutant.
            if mutation.is_unsafe(unsafe_targeting) { break 'mutant_candidate None; }

            mutants.iter_mut().find(|mutant| {
                // Ensure the mutant has not already reached capacity.
                if mutant.mutations.len() >= mutant_max_mutations_count { return false; }

                // The substitutions that make up each mutation of a mutant cannot conflict with each other.
                for subst in &mutation.substs {
                    if mutant.iter_substitutions().any(|s| conflicting_substs(s, subst)) { return false; }
                }

                // Mutations cannot be added to a mutant of an unsafe mutation.
                if mutant.mutations.iter().any(|m| m.is_unsafe(unsafe_targeting)) { return false; }

                // To discern results related to the various mutations of a mutant, they have to have distinct entry points.
                if mutant.mutations.iter().any(|m| conflicting_mutations(m.id, mutation.id)) { return false; }

                true
            })
        };

        match mutant_candidate {
            Some(mutant) => mutant.mutations.push(mutation),
            None => mutants.push(Mutant { mutations: vec![mutation] }),
        }
    }

    mutants
}
