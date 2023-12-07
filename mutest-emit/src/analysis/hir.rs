pub use rustc_hir::*;
pub use rustc_hir::def::*;
pub use rustc_hir::def_id::*;
pub use rustc_hir::definitions::*;

use rustc_hir as hir;
use rustc_middle::ty::TyCtxt;
use rustc_span::Span;
use rustc_span::symbol::Ident;

#[derive(Clone, Copy, Debug)]
pub struct FnItem<'hir> {
    pub owner_id: hir::OwnerId,
    pub span: Span,
    pub vis_span: Option<Span>,
    pub kind: hir::intravisit::FnKind<'hir>,
    pub ident: Ident,
    pub generics: &'hir hir::Generics<'hir>,
    pub sig: &'hir hir::FnSig<'hir>,
    pub body: &'hir hir::Body<'hir>,
}

impl<'tcx: 'hir, 'hir> FnItem<'hir> {
    pub fn from_node(tcx: TyCtxt<'tcx>, node: hir::Node<'hir>) -> Option<Self> {
        match node {
            hir::Node::Item(&hir::Item { owner_id, span, vis_span, ident, ref kind }) => {
                let hir::ItemKind::Fn(sig, generics, body) = kind else { unreachable!(); };
                let body = tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::ItemFn(ident, generics, sig.header);
                Some(FnItem { owner_id, span, ident, kind: fn_kind, vis_span: Some(vis_span), sig, generics, body })
            }
            hir::Node::TraitItem(&hir::TraitItem { owner_id, span, ident, ref generics, ref kind, defaultness: _ }) => {
                let hir::TraitItemKind::Fn(sig, hir::TraitFn::Provided(body)) = kind else { unreachable!(); };
                let body = tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::Method(ident, sig);
                Some(FnItem { owner_id, span, ident, kind: fn_kind, vis_span: None, sig, generics, body })
            }
            hir::Node::ImplItem(&hir::ImplItem { owner_id, span, vis_span, ident, ref generics, ref kind, defaultness: _ }) => {
                let hir::ImplItemKind::Fn(sig, body) = kind else { unreachable!(); };
                let body = tcx.hir().body(*body);
                let fn_kind = hir::intravisit::FnKind::Method(ident, sig);
                Some(FnItem { owner_id, span, ident, kind: fn_kind, vis_span: Some(vis_span), sig, generics, body })
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
