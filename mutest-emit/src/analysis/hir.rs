pub use rustc_hir::*;
pub use rustc_hir::def::*;
pub use rustc_hir::def_id::*;
pub use rustc_hir::definitions::*;

use rustc_ast as ast;
use rustc_hir as hir;
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;
use rustc_span::symbol::Ident;

use crate::analysis::Descr;

#[derive(Clone, Copy, Debug)]
pub struct FnItem<'hir> {
    pub owner_id: hir::OwnerId,
    pub span: Span,
    pub vis_span: Option<Span>,
    pub kind: hir::intravisit::FnKind<'hir>,
    pub ident: Ident,
    pub generics: &'hir hir::Generics<'hir>,
    pub sig: &'hir hir::FnSig<'hir>,
    pub body: Option<&'hir hir::Body<'hir>>,
}

impl<'tcx: 'hir, 'hir> FnItem<'hir> {
    pub fn from_node(tcx: TyCtxt<'tcx>, node: hir::Node<'hir>) -> Option<Self> {
        match node {
            hir::Node::Item(&hir::Item { owner_id, span, vis_span, ident, ref kind }) => {
                let hir::ItemKind::Fn(sig, generics, body) = kind else { return None; };
                let body = Some(tcx.hir().body(*body));
                let fn_kind = hir::intravisit::FnKind::ItemFn(ident, generics, sig.header);
                Some(FnItem { owner_id, span, ident, kind: fn_kind, vis_span: Some(vis_span), sig, generics, body })
            }
            hir::Node::TraitItem(&hir::TraitItem { owner_id, span, ident, ref generics, ref kind, defaultness: _ }) => {
                let hir::TraitItemKind::Fn(sig, trait_fn) = kind else { return None; };
                let body = match trait_fn {
                    hir::TraitFn::Provided(body) => Some(tcx.hir().body(*body)),
                    hir::TraitFn::Required(_param_idents) => None,
                };
                let fn_kind = hir::intravisit::FnKind::Method(ident, sig);
                Some(FnItem { owner_id, span, ident, kind: fn_kind, vis_span: None, sig, generics, body })
            }
            hir::Node::ImplItem(&hir::ImplItem { owner_id, span, vis_span, ident, ref generics, ref kind, defaultness: _ }) => {
                let hir::ImplItemKind::Fn(sig, body) = kind else { return None; };
                let body = Some(tcx.hir().body(*body));
                let fn_kind = hir::intravisit::FnKind::Method(ident, sig);
                Some(FnItem { owner_id, span, ident, kind: fn_kind, vis_span: Some(vis_span), sig, generics, body })
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ConstItem<'hir> {
    pub ty: &'hir hir::Ty<'hir>,
    pub generics: Option<&'hir hir::Generics<'hir>>,
    pub body: Option<&'hir hir::Body<'hir>>,
}

impl<'tcx: 'hir, 'hir> ConstItem<'hir> {
    pub fn from_node(tcx: TyCtxt<'tcx>, node: hir::Node<'hir>) -> Option<Self> {
        match node {
            hir::Node::Item(&hir::Item { ref kind, .. }) => {
                let hir::ItemKind::Const(ty, generics, body) = kind else { return None; };
                let body = Some(tcx.hir().body(*body));
                Some(ConstItem { ty, generics: Some(generics), body })
            }
            hir::Node::TraitItem(&hir::TraitItem { ref kind, .. }) => {
                let hir::TraitItemKind::Const(ty, body) = kind else { return None; };
                let body = body.map(|body| tcx.hir().body(body));
                Some(ConstItem { ty, generics: None, body })
            }
            hir::Node::ImplItem(&hir::ImplItem { ref kind, .. }) => {
                let hir::ImplItemKind::Const(ty, body) = kind else { return None; };
                let body = Some(tcx.hir().body(*body));
                Some(ConstItem { ty, generics: None, body })
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct StaticItem<'hir> {
    pub ty: &'hir hir::Ty<'hir>,
    pub mutability: ast::Mutability,
    pub body: Option<&'hir hir::Body<'hir>>,
}

impl<'tcx: 'hir, 'hir> StaticItem<'hir> {
    pub fn from_node(tcx: TyCtxt<'tcx>, node: hir::Node<'hir>) -> Option<Self> {
        match node {
            hir::Node::Item(&hir::Item { ref kind, .. }) => {
                let hir::ItemKind::Static(ty, mutability, body) = kind else { return None; };
                let body = Some(tcx.hir().body(*body));
                Some(StaticItem { ty, mutability: *mutability, body })
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TyAliasItem<'hir> {
    Item(&'hir hir::Ty<'hir>, &'hir hir::Generics<'hir>),
    TraitItem(hir::GenericBounds<'hir>, Option<&'hir hir::Ty<'hir>>),
    ImplItem(&'hir hir::Ty<'hir>),
}

impl<'hir> TyAliasItem<'hir> {
    pub fn from_node(node: hir::Node<'hir>) -> Option<Self> {
        match node {
            hir::Node::Item(&hir::Item { ref kind, .. }) => {
                let hir::ItemKind::TyAlias(ty, generics) = kind else { return None; };
                Some(TyAliasItem::Item(ty, generics))
            }
            hir::Node::TraitItem(&hir::TraitItem { ref kind, .. }) => {
                let hir::TraitItemKind::Type(generic_bounds, ty) = kind else { return None; };
                Some(TyAliasItem::TraitItem(generic_bounds, *ty))
            }
            hir::Node::ImplItem(&hir::ImplItem { ref kind, .. }) => {
                let hir::ImplItemKind::Type(ty) = kind else { return None; };
                Some(TyAliasItem::ImplItem(ty))
            }
            _ => None,
        }
    }

    pub fn ty(&self) -> Option<&'hir hir::Ty<'hir>> {
        match self {
            Self::Item(ty, _) => Some(ty),
            Self::TraitItem(_, ty) => *ty,
            Self::ImplItem(ty) => Some(ty),
        }
    }
}

pub trait NodeExt<'hir> {
    fn qpath(&self) -> Option<&'hir hir::QPath<'hir>>;
}

impl<'hir> NodeExt<'hir> for hir::Node<'hir> {
    fn qpath(&self) -> Option<&'hir hir::QPath<'hir>> {
        match self {
            hir::Node::Expr(expr_hir) => {
                match &expr_hir.kind {
                    hir::ExprKind::Path(qpath_hir) => Some(qpath_hir),
                    hir::ExprKind::Struct(qpath_hir, _, _) => Some(qpath_hir),
                    _ => None,
                }
            }
            hir::Node::Pat(pat_hir) => {
                match &pat_hir.kind {
                    hir::PatKind::Path(qpath_hir) => Some(qpath_hir),
                    hir::PatKind::Struct(qpath_hir, _, _) => Some(qpath_hir),
                    hir::PatKind::TupleStruct(qpath_hir, _, _) => Some(qpath_hir),
                    _ => None,
                }
            }
            hir::Node::Ty(ty_hir) => {
                match &ty_hir.kind {
                    hir::TyKind::Path(qpath_hir) => Some(qpath_hir),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum DefItem<'hir> {
    Item(&'hir hir::Item<'hir>),
    ForeignItem(&'hir hir::ForeignItem<'hir>),
    TraitItem(&'hir hir::TraitItem<'hir>),
    ImplItem(&'hir hir::ImplItem<'hir>),
}

impl<'hir> DefItem<'hir> {
    pub fn from_node(node: &'hir hir::Node<'hir>) -> Option<Self> {
        match node {
            hir::Node::Item(item) => Some(Self::Item(item)),
            hir::Node::ForeignItem(item) => Some(Self::ForeignItem(item)),
            hir::Node::TraitItem(item) => Some(Self::TraitItem(item)),
            hir::Node::ImplItem(item) => Some(Self::ImplItem(item)),
            _ => None,
        }
    }

    pub fn def_id(&self) -> LocalDefId {
        match self {
            Self::Item(item) => item.owner_id.def_id,
            Self::ForeignItem(item) => item.owner_id.def_id,
            Self::TraitItem(item) => item.owner_id.def_id,
            Self::ImplItem(item) => item.owner_id.def_id,
        }
    }

    pub fn ident(&self) -> Ident {
        match self {
            Self::Item(item) => item.ident,
            Self::ForeignItem(item) => item.ident,
            Self::TraitItem(item) => item.ident,
            Self::ImplItem(item) => item.ident,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Item(item) => item.span,
            Self::ForeignItem(item) => item.span,
            Self::TraitItem(item) => item.span,
            Self::ImplItem(item) => item.span,
        }
    }
}

impl<'hir> Descr for hir::StmtKind<'hir> {
    fn descr(&self) -> &'static str {
        match self {
            hir::StmtKind::Item(..) => "item",
            hir::StmtKind::Let(..) => "let",
            hir::StmtKind::Semi(..) => "statement expression",
            hir::StmtKind::Expr(..) => "trailing expression",
        }
    }
}

impl<'hir> Descr for hir::ExprKind<'hir> {
    fn descr(&self) -> &'static str {
        match self {
            hir::ExprKind::ConstBlock(..) => "const block",
            hir::ExprKind::Array(..) => "array literal",
            hir::ExprKind::Call(..) => "call",
            hir::ExprKind::MethodCall(..) => "method call",
            hir::ExprKind::Tup(..) => "tuple literal",
            hir::ExprKind::Binary(..) => "binary operation",
            hir::ExprKind::Unary(..) => "unary operation",
            hir::ExprKind::Lit(..) => "literal",
            hir::ExprKind::Cast(..) => "cast",
            hir::ExprKind::Type(..) => "type ascription",
            hir::ExprKind::DropTemps(..) => "drop temporaries",
            hir::ExprKind::Let(..) => "let",
            hir::ExprKind::If(..) => "if",
            hir::ExprKind::Loop(..) => "loop",
            hir::ExprKind::Match(..) => "match",
            hir::ExprKind::Closure(..) => "closure",
            hir::ExprKind::Block(..) => "block",
            hir::ExprKind::Assign(..) => "assignment",
            hir::ExprKind::AssignOp(..) => "assignment with operator",
            hir::ExprKind::Field(..) => "field access",
            hir::ExprKind::Index(..) => "index",
            hir::ExprKind::Path(..) => "path",
            hir::ExprKind::AddrOf(..) => "reference",
            hir::ExprKind::Break(..) => "break",
            hir::ExprKind::Continue(..) => "continue",
            hir::ExprKind::Ret(..) => "return",
            hir::ExprKind::Become(..) => "become",
            hir::ExprKind::InlineAsm(..) => "inline assembly",
            hir::ExprKind::OffsetOf(..) => "field offset",
            hir::ExprKind::Struct(..) => "struct literal",
            hir::ExprKind::Repeat(..) => "array from repetition",
            hir::ExprKind::Yield(..) => "yield",
            hir::ExprKind::Err(..) => "error",
        }
    }
}

impl<'hir> Descr for hir::PatKind<'hir> {
    fn descr(&self) -> &'static str {
        match self {
            hir::PatKind::Wild => "_",
            hir::PatKind::Never => "!",
            hir::PatKind::Lit(..) => "literal",
            hir::PatKind::Path(..) => "path",
            hir::PatKind::Binding(..) => "binding",
            hir::PatKind::Tuple(..) => "tuple",
            hir::PatKind::Struct(..) => "struct",
            hir::PatKind::TupleStruct(..) => "tuple struct",
            hir::PatKind::Box(..) => "box",
            hir::PatKind::Ref(..) => "reference",
            hir::PatKind::Deref(..) => "deref",
            hir::PatKind::Or(..) => "or",
            hir::PatKind::Range(..) => "range",
            hir::PatKind::Slice(..) => "slice",
            hir::PatKind::Err(..) => "error",
        }
    }
}

impl<'hir> Descr for hir::TyKind<'hir> {
    fn descr(&self) -> &'static str {
        match self {
            hir::TyKind::Never => "!",
            hir::TyKind::Path(..) => "path",
            hir::TyKind::Ptr(..) => "raw pointer",
            hir::TyKind::Ref(..) => "reference",
            hir::TyKind::Slice(..) => "slice",
            hir::TyKind::Array(..) => "array",
            hir::TyKind::Tup(..) => "tuple",
            hir::TyKind::BareFn(..) => "fn pointer",
            hir::TyKind::OpaqueDef(..) => "opaque definition",
            hir::TyKind::TraitObject(..) => "trait object",
            hir::TyKind::Typeof(..) => "typeof",
            hir::TyKind::Infer => "infer",
            hir::TyKind::InferDelegation(..) => "infer delegation",
            hir::TyKind::AnonAdt(..) => "anon adt",
            hir::TyKind::Pat(..) => "pattern",
            hir::TyKind::Err(..) => "error",
        }
    }
}
