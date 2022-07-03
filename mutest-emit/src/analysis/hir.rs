pub use rustc_hir::*;
pub use rustc_hir::def_id::*;

pub use inlined::*;
mod inlined {
    use rustc_hir as hir;
    use rustc_span::Span;
    use rustc_span::symbol::Ident;

    #[derive(Clone, Copy)]
    pub struct InlinedFn<'hir> {
        pub def_id: hir::def_id::LocalDefId,
        pub span: Span,
        pub ident: Ident,
        pub kind: hir::intravisit::FnKind<'hir>,
        pub vis_span: Option<Span>,
        pub generics: &'hir hir::Generics<'hir>,
        pub sig: &'hir hir::FnSig<'hir>,
        pub body: &'hir hir::Body<'hir>,
    }
}
