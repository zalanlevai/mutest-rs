use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

use rustc_hash::{FxHashSet, FxHashMap};
use rustc_session::Session;
use rustc_span::source_map::SourceMap;
use smallvec::{SmallVec, smallvec};

use crate::analysis::ast_lowering;
use crate::analysis::call_graph::{Target, UnsafeSource, Unsafety};
use crate::analysis::diagnostic;
use crate::analysis::hir;
use crate::analysis::res;
use crate::analysis::ty::TyCtxt;
use crate::codegen::ast::{self, P};
use crate::codegen::ast::visit::Visitor;
use crate::codegen::expansion::TcxExpansionExt;
use crate::codegen::substitution::conflicting_substs;
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, sym};
use crate::codegen::symbols::hygiene::AstPass;
use crate::codegen::tool_attr;
use crate::session::Options;

#[derive(Clone, Copy)]
pub enum MutLoc<'ast, 'a> {
    Fn(&'a ast::FnItem<'ast>),
    FnParam(&'a ast::Param, &'a ast::FnItem<'ast>),
    FnBodyStmt(&'a ast::Stmt, &'a ast::FnItem<'ast>),
    FnBodyExpr(&'a ast::Expr, &'a ast::FnItem<'ast>),
}

impl<'ast, 'a> MutLoc<'ast, 'a> {
    pub fn span(&self) -> Span {
        match self {
            Self::Fn(fn_item) => fn_item.span,
            Self::FnParam(param, _) => param.span,
            Self::FnBodyStmt(stmt, _) => stmt.span,
            Self::FnBodyExpr(expr, _) => expr.span,
        }
    }

    pub fn containing_fn(&self) -> Option<&ast::FnItem<'ast>> {
        match self {
            Self::Fn(fn_item) => Some(fn_item),
            Self::FnParam(_, fn_item) => Some(fn_item),
            Self::FnBodyStmt(_, fn_item) => Some(fn_item),
            Self::FnBodyExpr(_, fn_item) => Some(fn_item),
        }
    }
}

pub struct MutCtxt<'tcx, 'ast, 'op> {
    pub opts: &'op Options,
    pub tcx: TyCtxt<'tcx>,
    pub crate_res: &'op res::CrateResolutions<'tcx>,
    pub def_res: &'op ast_lowering::DefResolutions,
    pub body_res: &'op ast_lowering::BodyResolutions<'tcx>,
    pub def_site: Span,
    pub item_hir: &'op hir::FnItem<'tcx>,
    pub location: MutLoc<'ast, 'op>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum SubstLoc {
    InsertBefore(ast::NodeId),
    InsertAfter(ast::NodeId),
    Replace(ast::NodeId),
}

impl SubstLoc {
    pub fn is_dummy(&self) -> bool {
        match *self {
            Self::InsertBefore(node_id) => node_id == ast::DUMMY_NODE_ID,
            Self::InsertAfter(node_id) => node_id == ast::DUMMY_NODE_ID,
            Self::Replace(node_id) => node_id == ast::DUMMY_NODE_ID,
        }
    }
}

pub enum Subst {
    AstExpr(ast::Expr),
    AstStmt(ast::Stmt),
    AstLocal(Ident, ast::Mutability, Option<P<ast::Ty>>, P<ast::Expr>, Option<P<ast::Expr>>),
}

impl Subst {
    pub fn descr(&self) -> String {
        match self {
            Subst::AstExpr(_) => "expression".to_owned(),
            Subst::AstStmt(_) => "statement".to_owned(),
            Subst::AstLocal(ident, _, _, _, _) => format!("local `{ident}`"),
        }
    }

    pub fn to_source_string(&self) -> String {
        match self {
            Subst::AstExpr(expr) => ast::print::expr_to_string(expr),
            Subst::AstStmt(stmt) => ast::print::stmt_to_string(stmt),
            Subst::AstLocal(ident, mutbl, ty, init_expr, _default_expr) => {
                let local_stmt = ast::mk::stmt_local(DUMMY_SP, mutbl.is_mut(), *ident, ty.clone(), ast::LocalKind::Init(init_expr.clone()));
                ast::print::stmt_to_string(&local_stmt)
            }
        }
    }
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
    fn op_name(&self) -> &str;

    fn display_name(&self) -> String;

    fn span_label(&self) -> String {
        self.display_name()
    }
}

pub type MutWithSubsts<M> = (M, SmallVec<[SubstDef; 1]>);
pub type BoxedMutWithSubsts<M> = (Box<M>, SmallVec<[SubstDef; 1]>);

pub struct Mutations<M: Mutation>(SmallVec<[MutWithSubsts<M>; 1]>);

impl<M: Mutation> Mutations<M> {
    pub fn none() -> Self {
        Self(SmallVec::new())
    }

    pub fn new_one(mutation: M, substs: SmallVec<[SubstDef; 1]>) -> Self {
        Self(smallvec![(mutation, substs)])
    }

    pub fn new(mutations: SmallVec<[MutWithSubsts<M>; 1]>) -> Self {
        Self(mutations)
    }
}

pub type BoxedMutations<M> = SmallVec<[BoxedMutWithSubsts<M>; 1]>;

pub trait Operator<'a>: Send + Sync {
    type Mutation: Mutation + 'a;

    fn try_apply(&self, mcx: &MutCtxt) -> Mutations<Self::Mutation>;
}

pub trait OperatorBoxed<'a>: Send + Sync {
    type Mutation: Mutation + ?Sized + 'a;

    fn try_apply_boxed(&self, mcx: &MutCtxt) -> BoxedMutations<Self::Mutation>;
}

impl<'a, T: Operator<'a>> OperatorBoxed<'a> for T {
    type Mutation = dyn Mutation + 'a;

    fn try_apply_boxed(&self, mcx: &MutCtxt) -> BoxedMutations<Self::Mutation> {
        self.try_apply(mcx).0.into_iter()
            .map(|(mutation, substs)| {
                let mutation: Box<dyn Mutation + 'a> = Box::new(mutation);
                (mutation, substs)
            })
            .collect()
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

pub struct Mut<'trg, 'm> {
    pub id: MutId,
    pub target: &'trg Target<'trg>,
    pub span: Span,
    pub is_in_unsafe_block: bool,
    pub mutation: BoxedMutation<'m>,
    pub substs: SmallVec<[SubstDef; 1]>,
}

impl<'trg, 'm> Mut<'trg, 'm> {
    pub fn op_name(&self) -> &str {
        self.mutation.op_name()
    }

    pub fn display_name(&self) -> String {
        self.mutation.display_name()
    }

    pub fn display_location(&self, sess: &Session) -> String {
        sess.source_map().span_to_embeddable_string(self.span)
    }

    pub fn undetected_diagnostic(&self, sess: &Session) -> String {
        let mut diagnostic = sess.dcx().struct_span_warn(self.span, format!("[{op_name}] mutation was not detected",
            op_name = self.mutation.op_name(),
        ));
        diagnostic.span_label(self.span, self.mutation.span_label());

        for subst in &self.substs {
            let action = match &subst.location {
                SubstLoc::InsertBefore(_) | SubstLoc::InsertAfter(_) => "inserted",
                SubstLoc::Replace(_) => "replaced with",
            };
            let node_kind = subst.substitute.descr();
            let new_node = subst.substitute.to_source_string();
            diagnostic.note(format!("{node_kind} {action}: {new_node}",
                new_node = match new_node.lines().count() {
                    0 => "<empty>".to_owned(),
                    1 => format!("`{new_node}`"),
                    _ => format!("\n```\n{new_node}\n```"),
                },
            ));
        }

        diagnostic::emit_str(diagnostic, sess.psess.clone_source_map())
    }

    pub fn is_unsafe(&self, unsafe_targeting: UnsafeTargeting) -> bool {
        self.is_in_unsafe_block || self.target.unsafety.is_unsafe(unsafe_targeting)
    }
}

impl<'trg, 'm> Eq for Mut<'trg, 'm> {}
impl<'trg, 'm> PartialEq for Mut<'trg, 'm> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'trg, 'm> Hash for Mut<'trg, 'm> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

/// | Flag       | UnsafeTargeting       | `unsafe {}` | `{ unsafe {} }` | `{}` |
/// | ---------- | --------------------- | ----------- | --------------- | ---- |
/// | --safe     | None                  |             |                 | M    |
/// | --cautious | OnlyEnclosing(Unsafe) |             | unsafe M        | M    |
/// | --risky    | OnlyEnclosing(Normal) |             | M               | M    |
/// | --unsafe   | All                   | unsafe M    | M               | M    |
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnsafeTargeting {
    None,
    OnlyEnclosing(hir::Safety),
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

struct MutationCollector<'tcx, 'ast, 'op, 'trg, 'm> {
    operators: Operators<'op, 'm>,
    opts: &'op Options,
    tcx: TyCtxt<'tcx>,
    crate_res: &'op res::CrateResolutions<'tcx>,
    def_res: &'op ast_lowering::DefResolutions,
    body_res: &'op ast_lowering::BodyResolutions<'tcx>,
    def_site: Span,
    unsafe_targeting: UnsafeTargeting,
    target: Option<&'trg Target<'trg>>,
    current_fn: Option<(ast::FnItem<'ast>, hir::FnItem<'tcx>)>,
    current_closure: Option<hir::BodyId>,
    is_in_unsafe_block: bool,
    next_mut_index: u32,
    mutations: Vec<Mut<'trg, 'm>>,
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
            for (mutation, substs) in operator.try_apply_boxed(&mcx) {
                $self.mutations.push(Mut {
                    id: MutId($self.next_mut_index),
                    target: $self.target.expect("attempted to collect mutations without a target"),
                    span: mcx.location.span(),
                    is_in_unsafe_block: $self.is_in_unsafe_block,
                    mutation,
                    substs,
                });

                $self.next_mut_index += 1;
            }
        }
    }
}

fn is_local_span(source_map: &SourceMap, sp: Span) -> bool {
    let local_begin = source_map.lookup_byte_offset(sp.lo());
    let local_end = source_map.lookup_byte_offset(sp.hi());
    local_begin.sf.src.is_some() && local_end.sf.src.is_some()
}

fn report_unmatched_ast_node<'tcx>(tcx: TyCtxt<'tcx>, node_kind: &str, def_id: hir::LocalDefId, span: Span) {
    let mut diagnostic = tcx.dcx().struct_warn(format!("unmatched {node_kind} in {def_path}",
        def_path = tcx.def_path_debug_str(def_id.to_def_id()),
    ));
    diagnostic.span(span);
    diagnostic.span_label(span, "no matching HIR node found");
    diagnostic.emit();
}

impl<'tcx, 'ast, 'op, 'trg, 'm> ast::visit::Visitor<'ast> for MutationCollector<'tcx, 'ast, 'op, 'trg, 'm> {
    fn visit_fn(&mut self, kind: ast::visit::FnKind<'ast>, span: Span, id: ast::NodeId) {
        let ast::visit::FnKind::Fn(ctx, vis, fn_item) = kind else { return; };
        let fn_ast = ast::FnItem { id, span, ctx, vis, fn_data: fn_item };

        let Some(fn_def_id) = self.def_res.node_id_to_def_id.get(&fn_ast.id).copied() else { unreachable!() };
        let Some(fn_hir) = hir::FnItem::from_node(self.tcx, self.tcx.hir_node_by_def_id(fn_def_id)) else { unreachable!() };

        register_mutations!(self, MutCtxt {
            opts: self.opts,
            tcx: self.tcx,
            crate_res: self.crate_res,
            def_res: self.def_res,
            body_res: self.body_res,
            def_site: self.def_site,
            item_hir: &fn_hir,
            location: MutLoc::Fn(&fn_ast),
        });

        self.current_fn = Some((fn_ast, fn_hir));
        ast::visit::walk_fn(self, kind);
        self.current_fn = None;
    }

    fn visit_param(&mut self, param: &'ast ast::Param) {
        let Some((fn_ast, fn_hir)) = &self.current_fn else { return; };
        let Some(param_hir) = self.body_res.hir_param(param) else {
            if self.opts.verbosity >= 1 {
                report_unmatched_ast_node(self.tcx, "parameter", fn_hir.owner_id.def_id, param.span);
            }
            return;
        };

        if !is_local_span(self.tcx.sess.source_map(), param.span) { return; };
        if tool_attr::ignore(self.tcx.hir_attrs(param_hir.hir_id)) { return; }

        // FIXME: Nested function bodies are currently not represented in `MutLoc`, so we skip them for now to
        //        avoid generating leaking, malformed mutations.
        if let Some(_) = self.current_closure { return; }

        register_mutations!(self, MutCtxt {
            opts: self.opts,
            tcx: self.tcx,
            crate_res: self.crate_res,
            def_res: self.def_res,
            body_res: self.body_res,
            def_site: self.def_site,
            item_hir: fn_hir,
            location: MutLoc::FnParam(param, fn_ast),
        });

        ast::visit::walk_param(self, param);
    }

    fn visit_block(&mut self, block: &'ast ast::Block) {
        let Some((_fn_ast, fn_hir)) = &self.current_fn else { return; };
        let Some(block_hir) = self.body_res.hir_block(block) else {
            if self.opts.verbosity >= 1 {
                report_unmatched_ast_node(self.tcx, "block", fn_hir.owner_id.def_id, block.span);
            }
            return;
        };

        if !is_local_span(self.tcx.sess.source_map(), block.span) { return; };
        if tool_attr::ignore(self.tcx.hir_attrs(block_hir.hir_id)) { return; }
        if !self.unsafe_targeting.inside_unsafe() && let ast::BlockCheckMode::Unsafe(_) = block.rules { return; }

        let is_in_unsafe_block = self.is_in_unsafe_block;
        if let ast::BlockCheckMode::Unsafe(_) = block.rules { self.is_in_unsafe_block = true; }
        ast::visit::walk_block(self, block);
        if let ast::BlockCheckMode::Unsafe(_) = block.rules { self.is_in_unsafe_block = is_in_unsafe_block; }
    }

    fn visit_stmt(&mut self, stmt: &'ast ast::Stmt) {
        // The following nodes are not directly associated with any HIR node.
        match &stmt.kind {
            ast::StmtKind::Empty => return,
            // Nested item declarations are handled like top-level item declarations, and not as children.
            ast::StmtKind::Item(_item) => return,
            // Trailing expression statements are associated with the inner expression.
            ast::StmtKind::Expr(expr) => return self.visit_expr(expr),
            _ => {}
        }

        let Some((fn_ast, fn_hir)) = &self.current_fn else { return; };
        let Some(stmt_hir) = self.body_res.hir_stmt(stmt) else {
            if self.opts.verbosity >= 1 {
                report_unmatched_ast_node(self.tcx, "statement", fn_hir.owner_id.def_id, stmt.span);
            }
            return;
        };

        if !is_local_span(self.tcx.sess.source_map(), stmt.span) { return; };
        if tool_attr::ignore(self.tcx.hir_attrs(stmt_hir.hir_id)) { return; }

        // FIXME: Nested function bodies are currently not represented in `MutLoc`, so we skip them for now to
        //        avoid generating leaking, malformed mutations.
        if let Some(_) = self.current_closure { return; }

        register_mutations!(self, MutCtxt {
            opts: self.opts,
            tcx: self.tcx,
            crate_res: self.crate_res,
            def_res: self.def_res,
            body_res: self.body_res,
            def_site: self.def_site,
            item_hir: fn_hir,
            location: MutLoc::FnBodyStmt(stmt, fn_ast),
        });

        ast::visit::walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast ast::Expr) {
        // The following nodes are not directly associated with any HIR node.
        match &expr.kind {
            ast::ExprKind::Paren(expr) => return self.visit_expr(expr),
            _ => {}
        }

        let Some((fn_ast, fn_hir)) = &self.current_fn else { return; };
        let Some(expr_hir) = self.body_res.hir_expr(expr) else {
            if self.opts.verbosity >= 1 {
                report_unmatched_ast_node(self.tcx, "expression", fn_hir.owner_id.def_id, expr.span);
            }
            return;
        };

        if !is_local_span(self.tcx.sess.source_map(), expr.span) { return; };
        if tool_attr::ignore(self.tcx.hir_attrs(expr_hir.hir_id)) { return; }

        // FIXME: Nested function bodies are currently not represented in `MutLoc`, so we skip them for now to
        //        avoid generating leaking, malformed mutations.
        if let Some(_) = self.current_closure { return; }

        // Ignore block expressions with only a single nested node, visit the nested node instead.
        if let ast::ExprKind::Block(block_ast, _) = &expr.kind && block_ast.stmts.len() == 1 {
            return ast::visit::walk_expr(self, expr);
        }

        register_mutations!(self, MutCtxt {
            opts: self.opts,
            tcx: self.tcx,
            crate_res: self.crate_res,
            def_res: self.def_res,
            body_res: self.body_res,
            def_site: self.def_site,
            item_hir: fn_hir,
            location: MutLoc::FnBodyExpr(expr, fn_ast),
        });

        let current_closure = self.current_closure;
        if let hir::ExprKind::Closure(&hir::Closure { body, .. }) = expr_hir.kind { self.current_closure = Some(body); }

        match &expr.kind {
            // The left-hand side of assignment expressions only supports a strict subset of expressions, not including
            // the branching match expressions we use for substitutions, so we only mutate the right-hand side.
            ast::ExprKind::Assign(_lhs, rhs, _) | ast::ExprKind::AssignOp(_, _lhs, rhs) => {
                self.visit_expr(rhs);
            }
            // Only the matched expression, and each arm's guard and body expressions can be mutated.
            ast::ExprKind::Match(expr, arms, _) => {
                self.visit_expr(expr);
                for arm in arms {
                    if let Some(guard) = &arm.guard { self.visit_expr(guard); }
                    if let Some(body) = &arm.body { self.visit_expr(body); }
                }
            }
            // The `else` branch of an `if` conditional must be either another `if` conditional or a block, so we do
            // not mutate `else` blocks directly, instead visiting its contents.
            ast::ExprKind::If(_, _, _) => {
                fn inner_visit_if<'ast, T: ast::visit::Visitor<'ast, Result = ()>>(visitor: &mut T, expr: &'ast ast::Expr) {
                    let ast::ExprKind::If(cond, then, els) = &expr.kind else { unreachable!() };

                    visitor.visit_expr(cond);
                    visitor.visit_block(then);
                    if let Some(els) = els {
                        match &els.kind {
                            ast::ExprKind::If(_, _, _) => inner_visit_if(visitor, els),
                            ast::ExprKind::Block(_, _) => ast::visit::walk_expr(visitor, els),
                            _ => unreachable!("the else branch of an if expression can only be another if (else if) or a block (else)"),
                        }
                    }
                }

                inner_visit_if(self, expr);
            }
            _ => ast::visit::walk_expr(self, expr),
        }

        if let hir::ExprKind::Closure(_) = expr_hir.kind { self.current_closure = current_closure; }
    }

    fn visit_pat(&mut self, pat: &'ast ast::Pat) {
        match &pat.kind {
            // NOTE: We do not want to visit pattern expressions (i.e. literal, const block, path), since
            //       we cannot introduce dynamic mutations in them.
            ast::PatKind::Expr(_) => {}
            _ => ast::visit::walk_pat(self, pat),
        }
    }

    fn visit_attribute(&mut self, _attr: &'ast ast::Attribute) {}

    fn visit_ty(&mut self, _ty: &'ast ast::Ty) {
        // NOTE: We do not want to visit types, specifically expressions within them, since
        //       we cannot introduce dynamic mutations in them.
    }

    fn visit_anon_const(&mut self, _anon_const: &'ast ast::AnonConst) {
        // NOTE: We do not want to visit anonymous consts, specifically expressions within them, since
        //       we cannot introduce dynamic mutations in them.
    }
}

pub fn apply_mutation_operators<'ast, 'tcx, 'trg, 'm>(
    tcx: TyCtxt<'tcx>,
    crate_res: &res::CrateResolutions<'tcx>,
    def_res: &ast_lowering::DefResolutions,
    body_res: &ast_lowering::BodyResolutions<'tcx>,
    krate: &'ast ast::Crate,
    targets: impl Iterator<Item = &'trg Target<'trg>>,
    ops: Operators<'_, 'm>,
    unsafe_targeting: UnsafeTargeting,
    opts: &Options,
) -> Vec<Mut<'trg, 'm>> {
    let expn_id = tcx.expansion_for_ast_pass(
        AstPass::TestHarness,
        DUMMY_SP,
        &[sym::rustc_attrs],
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let mut collector = MutationCollector {
        operators: ops,
        opts,
        tcx,
        crate_res,
        def_res,
        body_res,
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

        let Some(target_item) = ast_lowering::find_def_in_ast(tcx, def_res, target.def_id, krate) else { continue; };

        match target_item {
            ast::DefItem::Item(item) => collector.visit_item(item),
            ast::DefItem::ForeignItem(item) => collector.visit_foreign_item(item),
            ast::DefItem::AssocItem(item, ctx) => collector.visit_assoc_item(item, ctx),
        }
    }

    collector.mutations
}

pub enum MutationError<'trg, 'm> {
    DummySubsts(&'m Mut<'trg, 'm>, Vec<&'m SubstDef>),
}

pub fn validate_mutations<'trg, 'm>(mutations: &'m [Mut<'trg, 'm>]) -> Result<(), Vec<MutationError<'trg, 'm>>> {
    use MutationError::*;

    let mut errors = vec![];

    for mutation in mutations {
        let dummy_substs = mutation.substs.iter().filter(|s| s.location.is_dummy()).collect::<Vec<_>>();
        if !dummy_substs.is_empty() {
            errors.push(DummySubsts(mutation, dummy_substs));
        }
    }

    if errors.is_empty() { return Ok(()) }
    Err(errors)
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct MutantId(u32);

impl MutantId {
    pub fn index(&self) -> u32 {
        self.0
    }
}

pub struct Mutant<'trg, 'm> {
    pub id: MutantId,
    pub mutations: Vec<Mut<'trg, 'm>>,
}

impl<'trg, 'm> Mutant<'trg, 'm> {
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

pub struct MutationConflictGraph<'m> {
    n_mutations: u32,
    unsafes: FxHashSet<MutId>,
    conflicts: FxHashSet<(MutId, MutId)>,
    phantom: PhantomData<&'m MutId>,
}

impl<'m> MutationConflictGraph<'m> {
    pub fn is_unsafe(&self, v: MutId) -> bool {
        self.unsafes.contains(&v)
    }

    pub fn conflicting_mutations(&self, a: MutId, b: MutId) -> bool {
        self.conflicts.contains(&(a, b)) || self.conflicts.contains(&(b, a))
    }

    pub fn compatible_mutations(&self, a: MutId, b: MutId) -> bool {
        !self.conflicting_mutations(a, b)
    }

    pub fn iter_conflicts(&self) -> impl Iterator<Item = (MutId, MutId)> + '_ {
        self.conflicts.iter().map(|&(a, b)| (a, b))
    }

    pub fn iter_conflicts_excluding_unsafe(&self) -> impl Iterator<Item = (MutId, MutId)> + '_{
        self.iter_conflicts().filter(|&(a, b)| !self.is_unsafe(a) && !self.is_unsafe(b))
    }

    pub fn iter_compatibilities(&self) -> MutationConflictGraphCompatibilityIter<'m, '_> {
        MutationConflictGraphCompatibilityIter::new(self)
    }
}

pub struct MutationConflictGraphCompatibilityIter<'m, 'op> {
    graph: &'op MutationConflictGraph<'m>,
    cursor: (u32, u32),
}

impl<'m, 'op> MutationConflictGraphCompatibilityIter<'m, 'op> {
    fn new(graph: &'op MutationConflictGraph<'m>) -> Self {
        Self { graph, cursor: (1, 2) }
    }
}

impl<'m, 'op> Iterator for MutationConflictGraphCompatibilityIter<'m, 'op> {
    type Item = (MutId, MutId);

    fn next(&mut self) -> Option<Self::Item> {
        while let (i, _) = self.cursor && i <= self.graph.n_mutations {
            while let (_, j) = self.cursor && j <= self.graph.n_mutations {
                self.cursor.1 += 1;

                if self.graph.compatible_mutations(MutId(i), MutId(j)) {
                    return Some((MutId(i), MutId(j)));
                }
            }

            self.cursor.0 += 1;
            self.cursor.1 = self.cursor.0 + 1;
        }

        None
    }
}

pub fn generate_mutation_conflict_graph<'trg, 'm>(mutations: &[Mut<'trg, 'm>], unsafe_targeting: UnsafeTargeting) -> MutationConflictGraph<'m> {
    let mut unsafes: FxHashSet<MutId> = Default::default();
    let mut conflicts: FxHashSet<(MutId, MutId)> = Default::default();

    let mut iterator = mutations.iter();
    while let Some(mutation) = iterator.next() {
        if mutation.is_unsafe(unsafe_targeting) {
            unsafes.insert(mutation.id);
        }

        for other in iterator.clone() {
            let is_conflicting = false
                // Unsafe mutations cannot be batched with any other mutation.
                || mutation.is_unsafe(unsafe_targeting)
                || other.is_unsafe(unsafe_targeting)
                // To discern results related to the various mutations of a mutant, they have to have distinct entry points.
                || conflicting_targets(&mutation.target, &other.target)
                // The substitutions that make up each mutation cannot conflict with each other.
                || mutation.substs.iter().any(|s| other.substs.iter().any(|s_other| conflicting_substs(s, s_other)));

            if is_conflicting {
                conflicts.insert((mutation.id, other.id));
            }
        }
    }

    let n_mutations = mutations.iter().map(|m| m.id.index()).max().unwrap_or(0);

    MutationConflictGraph { n_mutations, unsafes, conflicts, phantom: PhantomData }
}

pub enum MutationBatchesValidationError<'trg, 'm> {
    ConflictingMutationsInBatch(&'m Mutant<'trg, 'm>, SmallVec<[&'m Mut<'trg, 'm>; 2]>),
}

pub fn validate_mutation_batches<'trg, 'm>(mutants: &'m [Mutant<'trg, 'm>], mutation_conflict_graph: &MutationConflictGraph<'m>) -> Result<(), Vec<MutationBatchesValidationError<'trg, 'm>>> {
    use MutationBatchesValidationError::*;

    let mut errors = vec![];

    for mutant in mutants {
        let mut iterator = mutant.mutations.iter();
        while let Some(mutation) = iterator.next() {
            for other in iterator.clone() {
                if mutation_conflict_graph.conflicting_mutations(mutation.id, other.id) {
                    errors.push(ConflictingMutationsInBatch(mutant, smallvec![mutation, other]))
                }
            }
        }
    }

    if errors.is_empty() { return Ok(()) }
    Err(errors)
}

pub fn batch_mutations_dummy<'trg, 'm>(mutations: Vec<Mut<'trg, 'm>>) -> Vec<Mutant<'trg, 'm>> {
    let mut mutants: Vec<Mutant<'trg, 'm>> = Vec::with_capacity(mutations.len());
    let mut next_mutant_index = 1;

    for mutation in mutations {
        mutants.push(Mutant { id: MutantId(next_mutant_index), mutations: vec![mutation] });
        next_mutant_index += 1;
    }

    mutants
}

fn compatible_mutant<'trg, 'm>(
    mutation: &Mut<'trg, 'm>,
    mutant: &Mutant<'trg, 'm>,
    mutation_conflict_graph: &MutationConflictGraph<'m>,
    mutant_max_mutations_count: usize,
) -> bool {
    // Ensure the mutant has not already reached capacity.
    if mutant.mutations.len() >= mutant_max_mutations_count { return false; }

    // The mutation must not conflict with any other mutation already in the mutant.
    if mutant.mutations.iter().any(|m| mutation_conflict_graph.conflicting_mutations(m.id, mutation.id)) { return false; }

    true
}

/// Pick a compatible mutant for the mutation by randomly picking one from all compatible mutants.
/// If no compatible mutants are available, then the function returns `None`.
fn choose_random_mutant<'trg, 'm, 'a>(
    mutation: &Mut<'trg, 'm>,
    mutants: &'a mut [Mutant<'trg, 'm>],
    mutation_conflict_graph: &MutationConflictGraph<'m>,
    mutant_max_mutations_count: usize,
    rng: &mut impl rand::Rng,
) -> Option<&'a mut Mutant<'trg, 'm>> {
    use rand::prelude::*;

    if mutants.is_empty() { return None; }

    // Unsafe mutations are isolated into their own mutant.
    if mutation_conflict_graph.is_unsafe(mutation.id) { return None; }

    // Filter out mutants so that we only consider compatible mutants.
    let possible_mutants = mutants.iter_mut().filter(|mutant| compatible_mutant(mutation, mutant, mutation_conflict_graph, mutant_max_mutations_count));
    // Choose random compatible mutant, if available.
    possible_mutants.choose_stable(rng)
}

#[derive(Clone, Copy)]
pub enum GreedyMutationBatchingOrderingHeuristic {
    ConflictsAsc,
    ConflictsDesc,
    Random,
}

pub fn batch_mutations_greedy<'trg, 'm>(
    mut mutations: Vec<Mut<'trg, 'm>>,
    mutation_conflict_graph: &MutationConflictGraph<'m>,
    ordering_heuristic: Option<GreedyMutationBatchingOrderingHeuristic>,
    epsilon: Option<f64>,
    mut rng: Option<&mut impl rand::Rng>,
    mutant_max_mutations_count: usize,
) -> Vec<Mutant<'trg, 'm>> {
    use GreedyMutationBatchingOrderingHeuristic::*;
    match ordering_heuristic {
        Some(ConflictsAsc | ConflictsDesc) => {
            let mutation_conflict_heuristic = mutations.iter()
                .map(|mutation| {
                    let mut conflict_heuristic = 0_usize;
                    for other in &mutations {
                        if mutation == other { continue; }
                        if mutation_conflict_graph.conflicting_mutations(mutation.id, other.id) {
                            conflict_heuristic += 1;
                        }
                    }
                    (mutation.id, conflict_heuristic)
                })
                .collect::<FxHashMap<_, _>>();

            mutations.sort_by(|a, b| Ord::cmp(&mutation_conflict_heuristic.get(&a.id), &mutation_conflict_heuristic.get(&b.id)));
            if let Some(ConflictsDesc) = ordering_heuristic {
                mutations.reverse();
            }
        }

        Some(Random) => {
            use rand::prelude::*;
            let Some(ref mut rng) = rng else { panic!("random ordering requested but rng not provided") };

            mutations.shuffle(rng);
        }

        None => {}
    }

    let mut mutants: Vec<Mutant<'trg, 'm>> = vec![];
    let mut next_mutant_index = 1;

    for mutation in mutations {
        let mutant_candidate = 'mutant_candidate: {
            // Unsafe mutations are isolated into their own mutant.
            if mutation_conflict_graph.is_unsafe(mutation.id) { break 'mutant_candidate None; }

            // Attempt to make a random choice if the optional epsilon parameter is used.
            if let Some(epsilon) = epsilon {
                let Some(ref mut rng) = rng else { panic!("epsilon random choice requested but rng not provided") };

                // When using epsilon greedy batching, a random choice with probability epsilon is made for every
                // mutation. If the random choice is true, then instead of making a greedy choice, a random compatible
                // mutant is picked the same way as in random batching.
                if rng.random_bool(epsilon) {
                    break 'mutant_candidate choose_random_mutant(&mutation, &mut mutants, mutation_conflict_graph, mutant_max_mutations_count, rng);
                }
            }

            // Pick the first mutant the current mutation is compatible with.
            mutants.iter_mut().find(|mutant| compatible_mutant(&mutation, mutant, mutation_conflict_graph, mutant_max_mutations_count))
        };

        match mutant_candidate {
            Some(mutant) => mutant.mutations.push(mutation),
            None => {
                mutants.push(Mutant { id: MutantId(next_mutant_index), mutations: vec![mutation] });
                next_mutant_index += 1;
            }
        }
    }

    mutants
}

pub fn batch_mutations_random<'trg, 'm>(
    mutations: Vec<Mut<'trg, 'm>>,
    mutation_conflict_graph: &MutationConflictGraph<'m>,
    mutant_max_mutations_count: usize,
    rng: &mut impl rand::Rng,
) -> Vec<Mutant<'trg, 'm>> {
    let mut mutants: Vec<Mutant<'trg, 'm>> = vec![];
    let mut next_mutant_index = 1;

    for mutation in mutations {
        let mutant_candidate = choose_random_mutant(&mutation, &mut mutants, mutation_conflict_graph, mutant_max_mutations_count, rng);

        match mutant_candidate {
            Some(mutant) => mutant.mutations.push(mutation),
            None => {
                mutants.push(Mutant { id: MutantId(next_mutant_index), mutations: vec![mutation] });
                next_mutant_index += 1;
            }
        }
    }

    mutants
}

pub fn optimize_batches_simulated_annealing<'trg, 'm>(
    mutants: &mut Vec<Mutant<'trg, 'm>>,
    mutation_conflict_graph: &MutationConflictGraph<'m>,
    mutant_max_mutations_count: usize,
    max_iterations: usize,
    rng: &mut impl rand::Rng,
) {
    #[derive(Clone, Copy)]
    enum StateChange {
        MoveMutation { mutation_id: MutId, old_mutant_id: MutantId, new_mutant_id: MutantId },
    }

    impl StateChange {
        fn apply<'trg, 'm>(&self, mutants: &mut Vec<Mutant<'trg, 'm>>) {
            match self {
                Self::MoveMutation { mutation_id, old_mutant_id, new_mutant_id } => {
                    let Some(old_mutant_idx) = mutants.iter().position(|m| m.id == *old_mutant_id) else { unreachable!(); };
                    let old_mutant = &mut mutants[old_mutant_idx];
                    let Some(mutation) = old_mutant.mutations.extract_if(.., |m| m.id == *mutation_id).next() else { unreachable!() };
                    if old_mutant.mutations.is_empty() { mutants.remove(old_mutant_idx); }

                    let Some(new_mutant) = mutants.iter_mut().find(|m| m.id == *new_mutant_id) else { unreachable!(); };
                    new_mutant.mutations.push(mutation);
                }
            }
        }
    }

    fn random_neighbour_state<'trg, 'm>(
        mutants: &mut Vec<Mutant<'trg, 'm>>,
        mutation_conflict_graph: &MutationConflictGraph<'m>,
        mutant_max_mutations_count: usize,
        rng: &mut impl rand::Rng,
    ) -> StateChange {
        use rand::prelude::*;

        let (random_mutant, random_mutation, new_random_mutant) = loop {
            let Some(random_mutant) = mutants.choose(rng) else { unreachable!(); };
            let Some(random_mutation) = random_mutant.mutations.choose(rng) else { unreachable!(); };

            let compatible_mutants = mutants.iter()
                .filter(|m| m.id != random_mutant.id)
                .filter(|m| compatible_mutant(random_mutation, m, mutation_conflict_graph, mutant_max_mutations_count));
            let Some(new_random_mutant) = compatible_mutants.choose_stable(rng) else { continue; };

            break (random_mutant, random_mutation, new_random_mutant);
        };

        StateChange::MoveMutation { mutation_id: random_mutation.id, old_mutant_id: random_mutant.id, new_mutant_id: new_random_mutant.id }
    }

    fn temperature(iterations_expended: f64, max_iterations: usize) -> f64 {
        max_iterations as f64 * (1_f64 - iterations_expended)
    }

    fn energy<'trg, 'm>(mutants: &[Mutant<'trg, 'm>], state_change: Option<StateChange>) -> f64 {
        let mut mutants_count = mutants.len();

        if let Some(state_change) = state_change {
            match state_change {
                StateChange::MoveMutation { mutation_id: _, old_mutant_id, new_mutant_id: _ } => {
                    let Some(old_mutant) = mutants.iter().find(|m| m.id == old_mutant_id) else { unreachable!(); };
                    if old_mutant.mutations.len() <= 1 { mutants_count -= 1; }
                }
            }
        }

        mutants_count as f64
    }

    fn acceptance_probability(curr_energy: f64, next_energy: f64, temp: f64) -> f64 {
        if next_energy < curr_energy { return 1_f64; }
        f64::exp(-(next_energy - curr_energy) / temp)
    }

    for i in 0..max_iterations {
        let temp = temperature(1_f64 - ((i + 1) as f64 / max_iterations as f64), max_iterations);

        let curr_energy = energy(mutants, None);

        let state_change = random_neighbour_state(mutants, mutation_conflict_graph, mutant_max_mutations_count, rng);
        let next_energy = energy(mutants, Some(state_change));
        if acceptance_probability(curr_energy, next_energy, temp) >= rng.random_range(0_f64..1_f64) {
            state_change.apply(mutants);
        }
    }
}
