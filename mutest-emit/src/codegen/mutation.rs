use std::hash::{Hash, Hasher};

use rustc_expand::base::ExtCtxt;
use rustc_span::hygiene::AstPass;
use smallvec::SmallVec;

use crate::codegen::ast;
use crate::codegen::ast::visit::Visitor;
use crate::codegen::symbols::{DUMMY_SP, Ident, Span, Symbol, sym};

// TODO: Add documentation referencing `rustc_ast::visit::Fn`.
pub struct Fn<'a> {
    pub id: ast::NodeId,
    pub ctx: ast::visit::FnCtxt,
    pub ident: Ident,
    pub sig: &'a ast::FnSig,
    pub vis: &'a ast::Visibility,
    pub body: Option<&'a ast::Block>,
}

pub struct Lowered<'a, A, H> {
    pub ast: &'a A,
    pub hir: &'a H,
}

pub enum MutLoc<'a> {
    Fn(&'a Fn<'a>),
    FnParam(&'a ast::Param, &'a Fn<'a>),
    FnBodyStmt(&'a ast::Stmt, &'a Fn<'a>),
    FnBodyExpr(&'a ast::Expr, &'a Fn<'a>),
}

pub struct MutCtxt<'a> {
    pub def_site: Span,
    pub location: MutLoc<'a>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum SubstLoc {
    // InsertBeforeItem(&'a ast::Item),
    // InsertAfterItem(&'a ast::Item),
    // ReplaceItem(&'a ast::Item),
    // InsertBeforeStmt(&'a ast::Stmt),
    // InsertAfterStmt(&'a ast::Stmt),
    // ReplaceStmt(&'a ast::Stmt),
    // ReplaceExpr(&'a ast::Expr),
    InsertBefore(ast::NodeId),
    InsertAfter(ast::NodeId),
    Replace(ast::NodeId),
}

pub enum Subst {
    AstExpr(ast::Expr),
    AstStmt(ast::Stmt),
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

pub trait Mutation {}

pub trait Operator<'a> {
    type Mutation: mutest_bridge::Mutation + 'a;

    fn try_apply(&self, mcx: &MutCtxt) -> Option<(Self::Mutation, SmallVec<[SubstDef; 1]>)>;
}

pub trait OperatorBoxed<'a> {
    type Mutation: mutest_bridge::Mutation + ?Sized + 'a;

    // fn try_apply_boxed(&self, mcx: &MutCtxt) -> Option<(Box<dyn mutest_bridge::Mutation + 'a>, SmallVec<[SubstDef; 1]>)>;
    fn try_apply_boxed(&self, mcx: &MutCtxt) -> Option<(Box<Self::Mutation>, SmallVec<[SubstDef; 1]>)>;
}

impl<'a, T: Operator<'a>> OperatorBoxed<'a> for T {
    // type Mutation = T::Mutation;
    type Mutation = dyn mutest_bridge::Mutation + 'a;

    // fn try_apply_boxed(&self, mcx: &MutCtxt) -> Option<(Box<dyn mutest_bridge::Mutation + 'a>, SmallVec<[SubstDef; 1]>)>  {
    fn try_apply_boxed(&self, mcx: &MutCtxt) -> Option<(Box<Self::Mutation>, SmallVec<[SubstDef; 1]>)>  {
        match self.try_apply(mcx) {
            Some((mutation, substs)) => Some((Box::new(mutation), substs)),
            None => None,
        }
    }
}

pub type Operators<'op, 'm> = Vec<&'op dyn OperatorBoxed<'m, Mutation = dyn mutest_bridge::Mutation + 'm>>;
pub type BoxedMutation<'m> = Box<dyn mutest_bridge::Mutation + 'm>;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct MutId(u32);

impl MutId {
    pub fn into_symbol_name(&self) -> String {
        format!("mut_{}", self.0)
    }

    pub fn into_symbol(&self) -> Symbol {
        Symbol::intern(&self.into_symbol_name())
    }
}

pub struct Mut<'m> {
    pub id: MutId,
    pub mutation: BoxedMutation<'m>,
    pub substs: SmallVec<[SubstDef; 1]>,
}

impl<'m> Eq for Mut<'m> {}
impl<'m> PartialEq for Mut<'m> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<'m> Hash for Mut<'m> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

struct MutationCollector<'a, 'op, 'm> {
    pub operators: Operators<'op, 'm>,
    pub def_site: Span,
    pub current_fn: Option<Fn<'a>>,
    pub next_mut_index: u32,
    pub mutations: Vec<Mut<'m>>,
}

/// Macro used during mutation collection to apply every mutation operator using the given mutation
/// context.
///
/// This is a macro because the equivalent associated function would require a mutable borrow of
/// the entire `self` struct, which would disallow certain patterns used in the AST visitor.
macro register_mutations($self:ident, $($mcx:tt)+) {
    {
        let mcx = $($mcx)+;

        for operator in &$self.operators {
            if let Some((mutation, substs)) = operator.try_apply_boxed(&mcx) {
                $self.mutations.push(Mut {
                    id: MutId($self.next_mut_index),
                    mutation,
                    substs,
                });

                $self.next_mut_index += 1;
            }
        }
    }
}

impl<'a, 'op, 'm> ast::visit::Visitor<'a> for MutationCollector<'a, 'op, 'm> {
    fn visit_fn(&mut self, kind: ast::visit::FnKind<'a>, span: Span, id: ast::NodeId) {
        let ast::visit::FnKind::Fn(ctx, ident, sig, vis, body) = kind else {
            return ast::visit::walk_fn(self, kind, span);
        };

        let r#fn = Fn { id, ctx, ident, sig, vis, body };

        register_mutations!(self, MutCtxt {
            def_site: self.def_site,
            location: MutLoc::Fn(&r#fn),
        });

        // let fn_def_id = self.resolver.opt_local_def_id(r#fn.id);
        // println!("fn {}: {} (AST) -> {:#?} (HIR)", r#fn.ident, r#fn.id, fn_def_id);

        for param in &sig.decl.inputs {
            register_mutations!(self, MutCtxt {
                def_site: self.def_site,
                location: MutLoc::FnParam(&param, &r#fn),
            });
        }

        self.current_fn = Some(r#fn);
        ast::visit::walk_fn(self, kind, span);
        self.current_fn = None;
    }

    fn visit_stmt(&mut self, stmt: &'a ast::Stmt) {
        if let Some(ref r#fn) = self.current_fn {
            register_mutations!(self, MutCtxt {
                def_site: self.def_site,
                location: MutLoc::FnBodyStmt(stmt, r#fn),
            });
        }

        ast::visit::walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'a ast::Expr) {
        if let Some(ref r#fn) = self.current_fn {
            register_mutations!(self, MutCtxt {
                def_site: self.def_site,
                location: MutLoc::FnBodyExpr(expr, r#fn),
            });
        }

        ast::visit::walk_expr(self, expr);
    }
}

pub fn apply_mutation_operators<'m>(ecx: &mut ExtCtxt<'_>, ops: Operators<'_, 'm>, krate: &ast::Crate) -> Vec<Mut<'m>> {
    let expn_id = ecx.resolver.expansion_for_ast_pass(
        DUMMY_SP,
        AstPass::TestHarness,
        &[sym::rustc_attrs],
        None,
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    let mut collector = MutationCollector {
        operators: ops,
        def_site,
        current_fn: None,
        next_mut_index: 1,
        mutations: vec![],
    };
    collector.visit_crate(krate);

    collector.mutations
}

pub struct Mutant<'m> {
    pub mutations: Vec<Mut<'m>>,
}

impl<'m> Mutant<'m> {
    pub fn iter_mutations(&self) -> impl Iterator<Item = &BoxedMutation<'m>> {
        self.mutations.iter().map(|m| &m.mutation)
    }

    pub fn iter_substitutions(&self) -> impl Iterator<Item = &SubstDef> {
        self.mutations.iter().flat_map(|m| &m.substs)
    }
}

const MUTANT_MAX_MUTATIONS_COUNT: usize = 1;

pub fn batch_mutations<'m>(mutations: Vec<Mut<'m>>) -> Vec<Mutant<'m>> {
    let mut mutants: Vec<Mutant<'m>> = vec![];

    'mutation: for mutation in mutations {
        'mutant: for mutant in &mut mutants {
            for subst in &mutation.substs {
                if mutant.iter_substitutions().any(|s| s.location == subst.location) {
                    continue 'mutant;
                }
            }

            // HACK: Only "batch" a single mutation into a mutant for testing.
            if mutant.mutations.len() >= MUTANT_MAX_MUTATIONS_COUNT { break 'mutant; }

            mutant.mutations.push(mutation);
            continue 'mutation;
        }

        mutants.push(Mutant {
            mutations: vec![mutation],
        });
    }

    mutants
}
