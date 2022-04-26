pub use rustc_ast::*;
pub use rustc_ast::ptr::P;

pub use rustc_ast::token::TokenKind;
pub use rustc_ast::tokenstream::*;

pub use inlined::*;
mod inlined {
    use rustc_ast as ast;
    use rustc_span::Span;
    use rustc_span::symbol::Ident;

    // TODO: Add documentation referencing `rustc_ast::visit::Fn`.
    #[derive(Clone, Copy)]
    pub struct InlinedFn<'ast> {
        pub id: ast::NodeId,
        pub span: Span,
        pub ctx: ast::visit::FnCtxt,
        pub ident: Ident,
        pub sig: &'ast ast::FnSig,
        pub vis: &'ast ast::Visibility,
        pub body: Option<&'ast ast::Block>,
    }

    #[derive(Clone)]
    pub struct OwnedInlinedFn {
        pub id: ast::NodeId,
        pub span: Span,
        pub ctx: ast::visit::FnCtxt,
        pub ident: Ident,
        pub sig: ast::FnSig,
        pub vis: ast::Visibility,
        pub body: Option<ast::Block>,
    }

    impl<'ast> InlinedFn<'ast> {
        pub fn into_owned(&self) -> OwnedInlinedFn {
            OwnedInlinedFn {
                id: self.id,
                span: self.span,
                ctx: self.ctx,
                ident: self.ident,
                sig: self.sig.clone(),
                vis: self.vis.clone(),
                body: self.body.cloned(),
            }
        }
    }
}

pub mod mk {
    use std::iter;

    use rustc_ast as ast;
    use rustc_ast::ptr::P;
    use rustc_span::{Span, Symbol};
    use rustc_span::symbol::Ident;

    pub fn path_segment(sp: Span, ident: Ident, args: Vec<ast::GenericArg>) -> ast::PathSegment {
        let args = match !args.is_empty() {
            true => {
                let args = args.into_iter().map(ast::AngleBracketedArg::Arg).collect();
                ast::AngleBracketedArgs { span: sp, args }.into()
            }
            false => None,
        };

        ast::PathSegment {
            id: ast::DUMMY_NODE_ID,
            ident: ident.with_span_pos(sp),
            args,
        }
    }

    pub fn path_args(sp: Span, global: bool, mut idents: Vec<Ident>, args: Vec<ast::GenericArg>) -> ast::Path {
        assert!(!idents.is_empty());

        let add_root = global && !idents[0].is_path_segment_keyword();
        let mut segments = Vec::with_capacity(idents.len() + add_root as usize);
        if add_root {
            // NOTE: A path segment with an empty identifier is used instead of `ast::PathSegment::path_root`, because
            //       `ast::PathSegment::path_root` uses the `{{root}}` symbol which is improperly printed in token
            //       stream positions.
            segments.push(ast::PathSegment::from_ident(Ident::empty()));
        }

        let last_ident = idents.pop().unwrap();

        segments.extend(idents.into_iter().map(|ident| ast::PathSegment::from_ident(ident.with_span_pos(sp))));

        segments.push(self::path_segment(sp, last_ident, args));

        ast::Path { span: sp, segments, tokens: None }
    }

    pub fn path(sp: Span, global: bool, idents: Vec<Ident>) -> ast::Path {
        self::path_args(sp, global, idents, vec![])
    }

    pub fn path_global_args(sp: Span, idents: Vec<Ident>, args: Vec<ast::GenericArg>) -> ast::Path {
        self::path_args(sp, true, idents, args)
    }

    pub fn path_global(sp: Span, idents: Vec<Ident>) -> ast::Path {
        self::path(sp, true, idents)
    }

    pub fn path_ident(sp: Span, ident: Ident) -> ast::Path {
        self::path(sp, false, vec![ident])
    }

    pub fn path_symbols(sp: Span, symbols: Vec<Symbol>) -> ast::Path {
        let idents = symbols.into_iter().map(|s| Ident::new(s, sp)).collect::<Vec<_>>();
        self::path(sp, false, idents)
    }

    pub fn pathx_args(sp: Span, path: ast::Path, idents: Vec<Ident>, args: Vec<ast::GenericArg>) -> ast::Path {
        let idents = path.segments.iter()
            .map(|s| s.ident)
            .chain(idents.into_iter())
            .collect::<Vec<_>>();

        self::path_args(sp, false, idents, args)
    }

    pub fn pathx(sp: Span, path: ast::Path, idents: Vec<Ident>) -> ast::Path {
        self::pathx_args(sp, path, idents, vec![])
    }

    pub fn path_local(mut path: ast::Path) -> ast::Path {
        let Some(first_segment) = path.segments.first_mut() else { return path; };

        let is_global = !first_segment.ident.is_path_segment_keyword();
        if is_global { path.segments.remove(0); }

        path
    }

    pub fn ty(sp: Span, kind: ast::TyKind) -> P<ast::Ty> {
        P(ast::Ty { id: ast::DUMMY_NODE_ID, span: sp, kind, tokens: None })
    }

    pub fn ty_mut(ty: P<ast::Ty>, mutbl: ast::Mutability) -> ast::MutTy {
        ast::MutTy { ty, mutbl }
    }

    pub fn ty_path(q_self: Option<ast::QSelf>, path: ast::Path) -> P<ast::Ty> {
        self::ty(path.span, ast::TyKind::Path(q_self, path))
    }

    pub fn ty_ident(sp: Span, q_self: Option<ast::QSelf>, ident: Ident) -> P<ast::Ty> {
        self::ty_path(q_self, self::path_ident(sp, ident))
    }

    pub fn ty_rptr(sp: Span, ty: P<ast::Ty>, lifetime: Option<ast::Lifetime>, mutbl: ast::Mutability) -> P<ast::Ty> {
        self::ty(sp, ast::TyKind::Rptr(lifetime, self::ty_mut(ty, mutbl)))
    }

    pub fn ty_ref(sp: Span, ty: P<ast::Ty>, lifetime: Option<ast::Lifetime>) -> P<ast::Ty> {
        self::ty_rptr(sp, ty, lifetime, ast::Mutability::Not)
    }

    pub fn ty_mut_ref(sp: Span, ty: P<ast::Ty>, lifetime: Option<ast::Lifetime>) -> P<ast::Ty> {
        self::ty_rptr(sp, ty, lifetime, ast::Mutability::Mut)
    }

    pub fn ty_ptr(sp: Span, ty: P<ast::Ty>, mutbl: ast::Mutability) -> P<ast::Ty> {
        self::ty(sp, ast::TyKind::Ptr(self::ty_mut(ty, mutbl)))
    }

    pub fn ty_array(sp: Span, ty: P<ast::Ty>, length: ast::AnonConst) -> P<ast::Ty> {
        self::ty(sp, ast::TyKind::Array(ty, length))
    }

    pub fn ty_slice(sp: Span, ty: P<ast::Ty>) -> P<ast::Ty> {
        self::ty(sp, ast::TyKind::Slice(ty))
    }

    pub fn ty_param(sp: Span, ident: Ident, bounds: ast::GenericBounds, default: Option<P<ast::Ty>>) -> ast::GenericParam {
        ast::GenericParam {
            id: ast::DUMMY_NODE_ID,
            attrs: ast::AttrVec::new(),
            ident: ident.with_span_pos(sp),
            bounds,
            kind: ast::GenericParamKind::Type { default },
            is_placeholder: false,
        }
    }

    pub fn trait_ref(path: ast::Path) -> ast::TraitRef {
        ast::TraitRef { ref_id: ast::DUMMY_NODE_ID, path }
    }

    pub fn poly_trait_ref(sp: Span, path: ast::Path) -> ast::PolyTraitRef {
        ast::PolyTraitRef {
            span: sp,
            bound_generic_params: Vec::new(),
            trait_ref: self::trait_ref(path),
        }
    }

    pub fn trait_bound(path: ast::Path, modifier: ast::TraitBoundModifier) -> ast::GenericBound {
        ast::GenericBound::Trait(self::poly_trait_ref(path.span, path), modifier)
    }

    pub fn anon_const(sp: Span, kind: ast::ExprKind) -> ast::AnonConst {
        ast::AnonConst {
            id: ast::DUMMY_NODE_ID,
            value: P(ast::Expr {
                id: ast::DUMMY_NODE_ID,
                span: sp,
                attrs: ast::AttrVec::new(),
                kind,
                tokens: None,
            }),
        }
    }

    pub fn const_ident(sp: Span, ident: Ident) -> ast::AnonConst {
        self::anon_const(sp, ast::ExprKind::Path(None, self::path_ident(sp, ident)))
    }

    pub fn lifetime(sp: Span, ident: Ident) -> ast::Lifetime {
        ast::Lifetime { id: ast::DUMMY_NODE_ID, ident: ident.with_span_pos(sp) }
    }

    pub fn expr(sp: Span, kind: ast::ExprKind) -> P<ast::Expr> {
        P(ast::Expr {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            attrs: ast::AttrVec::new(),
            kind,
            tokens: None,
        })
    }

    pub fn expr_paren(sp: Span, expr: P<ast::Expr>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Paren(expr))
    }

    pub fn expr_path(path: ast::Path) -> P<ast::Expr> {
        self::expr(path.span, ast::ExprKind::Path(None, path))
    }

    pub fn expr_ident(sp: Span, id: Ident) -> P<ast::Expr> {
        self::expr_path(self::path_ident(sp, id))
    }

    pub fn expr_unary(sp: Span, op: ast::UnOp, expr: P<ast::Expr>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Unary(op, expr))
    }

    pub fn expr_not(sp: Span, expr: P<ast::Expr>) -> P<ast::Expr> {
        self::expr_unary(sp, ast::UnOp::Not, expr)
    }

    pub fn expr_deref(sp: Span, expr: P<ast::Expr>) -> P<ast::Expr> {
        self::expr_unary(sp, ast::UnOp::Deref, expr)
    }

    pub fn expr_addr_of(sp: Span, borrow: ast::BorrowKind, mutbl: ast::Mutability, expr: P<ast::Expr>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::AddrOf(borrow, mutbl, expr))
    }

    pub fn expr_ref(sp: Span, expr: P<ast::Expr>) -> P<ast::Expr> {
        self::expr_addr_of(sp, ast::BorrowKind::Ref, ast::Mutability::Not, expr)
    }

    pub fn expr_mut_ref(sp: Span, expr: P<ast::Expr>) -> P<ast::Expr> {
        self::expr_addr_of(sp, ast::BorrowKind::Ref, ast::Mutability::Mut, expr)
    }

    pub fn expr_binary(sp: Span, op: ast::BinOpKind, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Binary(ast::BinOp { span: sp, node: op }, lhs, rhs))
    }

    pub fn expr_assign(sp: Span, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Assign(lhs, rhs, sp))
    }

    pub fn expr_assign_op(sp: Span, op: ast::BinOpKind, lhs: P<ast::Expr>, rhs: P<ast::Expr>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::AssignOp(ast::BinOp { span: sp, node: op }, lhs, rhs))
    }

    pub fn pat(sp: Span, kind: ast::PatKind) -> P<ast::Pat> {
        P(ast::Pat { id: ast::DUMMY_NODE_ID, span: sp, kind, tokens: None })
    }

    pub fn pat_wild(sp: Span) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Wild)
    }

    pub fn pat_lit(sp: Span, expr: P<ast::Expr>) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Lit(expr))
    }

    pub fn pat_ident_binding_mode(sp: Span, ident: Ident, binding: ast::BindingMode) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Ident(binding, ident.with_span_pos(sp), None))
    }

    pub fn pat_ident(sp: Span, ident: Ident) -> P<ast::Pat> {
        self::pat_ident_binding_mode(sp, ident, ast::BindingMode::ByValue(ast::Mutability::Not))
    }

    pub fn pat_path(sp: Span, path: ast::Path) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Path(None, path))
    }

    pub fn pat_tuple(sp: Span, pats: Vec<P<ast::Pat>>) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Tuple(pats))
    }

    pub fn pat_tuple_struct(sp: Span, path: ast::Path, pats: Vec<P<ast::Pat>>) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::TupleStruct(None, path, pats))
    }

    pub fn pat_struct(sp: Span, path: ast::Path, field_pats: Vec<ast::PatField>, rest: bool) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Struct(None, path, field_pats, rest))
    }

    pub fn arm(sp: Span, pat: P<ast::Pat>, guard: Option<P<ast::Expr>>, expr: P<ast::Expr>) -> ast::Arm {
        ast::Arm {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            attrs: ast::AttrVec::new(),
            pat,
            guard,
            body: expr,
            is_placeholder: false,
        }
    }

    pub fn expr_match(span: Span, expr: P<ast::Expr>, arms: Vec<ast::Arm>) -> P<ast::Expr> {
        self::expr(span, ast::ExprKind::Match(expr, arms))
    }

    pub fn expr_if(sp: Span, cond: P<ast::Expr>, then: P<ast::Block>, els: Option<P<ast::Block>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::If(cond, then, els.map(self::expr_block)))
    }

    pub fn expr_call(sp: Span, expr: P<ast::Expr>, args: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Call(expr, args))
    }

    pub fn expr_call_ident(sp: Span, ident: Ident, args: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Call(self::expr_ident(sp, ident), args))
    }

    pub fn expr_call_path(sp: Span, path: ast::Path, args: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Call(self::expr_path(path), args))
    }

    pub fn expr_method_call(sp: Span, receiver: P<ast::Expr>, path: ast::PathSegment, args: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        let expr = iter::once(receiver).chain(args.into_iter()).collect::<Vec<_>>();
        self::expr(sp, ast::ExprKind::MethodCall(path, expr, sp))
    }

    pub fn expr_method_call_path_ident(sp: Span, receiver: ast::Path, ident: Ident, args: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr_method_call(sp, self::expr_path(receiver), self::path_segment(sp, ident, vec![]), args)
    }

    pub fn expr_field(sp: Span, expr: P<ast::Expr>, ident: Ident) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Field(expr, ident))
    }

    pub fn expr_field_deep(sp: Span, expr: P<ast::Expr>, idents: Vec<Ident>) -> P<ast::Expr> {
        idents.into_iter().fold(expr, |expr, ident| self::expr_field(sp, expr, ident))
    }

    pub fn expr_index(sp: Span, expr: P<ast::Expr>, index: P<ast::Expr>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Index(expr, index))
    }

    pub fn expr_block(block: P<ast::Block>) -> P<ast::Expr> {
        self::expr(block.span, ast::ExprKind::Block(block, None))
    }

    pub fn expr_noop(sp: Span) -> P<ast::Expr> {
        self::expr_block(self::block(sp, vec![]))
    }

    pub fn param(sp: Span, pat: P<ast::Pat>, ty: P<ast::Ty>) -> ast::Param {
        ast::Param {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            attrs: ast::AttrVec::new(),
            pat,
            ty,
            is_placeholder: false,
        }
    }

    pub fn param_ident(sp: Span, ident: Ident, ty: P<ast::Ty>) -> ast::Param {
        self::param(sp, self::pat_ident(sp, ident), ty)
    }

    pub fn fn_decl(inputs: Vec<ast::Param>, output: ast::FnRetTy) -> P<ast::FnDecl> {
        P(ast::FnDecl { inputs, output })
    }

    pub fn expr_closure(sp: Span, idents: Vec<Ident>, body: P<ast::Expr>) -> P<ast::Expr> {
        let fn_decl = self::fn_decl(
            idents.into_iter()
                .map(|ident| self::param(sp, self::pat_ident(sp, ident), self::ty(sp, ast::TyKind::Infer)))
                .collect(),
            ast::FnRetTy::Default(sp),
        );

        self::expr(sp, ast::ExprKind::Closure(
            ast::CaptureBy::Ref,
            ast::Async::No,
            ast::Movability::Movable,
            fn_decl,
            body,
            sp,
        ))
    }

    pub fn expr_struct_field(sp: Span, ident: Ident, expr: P<ast::Expr>) -> ast::ExprField {
        let is_shorthand = {
            if let ast::ExprKind::Path(None, ref path) = expr.kind
                && path.segments.len() == 1
                && let Some(segment) = path.segments.first()
            {
                segment.ident.name == ident.name
            } else {
                false
            }
        };

        ast::ExprField {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            attrs: ast::AttrVec::new(),
            ident: ident.with_span_pos(sp),
            expr,
            is_shorthand,
            is_placeholder: false,
        }
    }

    pub fn expr_struct(sp: Span, path: ast::Path, fields: Vec<ast::ExprField>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Struct(P(ast::StructExpr {
            qself: None,
            path,
            fields,
            rest: ast::StructRest::None,
        })))
    }

    pub fn expr_struct_ident(sp: Span, ident: Ident, fields: Vec<ast::ExprField>) -> P<ast::Expr> {
        self::expr_struct(sp, self::path_ident(sp, ident), fields)
    }

    pub fn expr_lit(sp: Span, kind: ast::LitKind) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Lit(ast::Lit::from_lit_kind(kind, sp)))
    }

    pub fn expr_bool(sp: Span, value: bool) -> P<ast::Expr> {
        self::expr_lit(sp, ast::LitKind::Bool(value))
    }

    pub fn expr_int(sp: Span, i: isize) -> P<ast::Expr> {
        let abs = self::expr_lit(sp, ast::LitKind::Int(i.abs() as u128, ast::LitIntType::Unsuffixed));

        match i {
            0.. => abs,
            _ => self::expr_unary(sp, ast::UnOp::Neg, abs)
        }
    }

    pub fn expr_usize(sp: Span, i: usize) -> P<ast::Expr> {
        self::expr_lit(sp, ast::LitKind::Int(i as u128, ast::LitIntType::Unsigned(ast::UintTy::Usize)))
    }

    pub fn expr_u32(sp: Span, i: u32) -> P<ast::Expr> {
        self::expr_lit(sp, ast::LitKind::Int(i as u128, ast::LitIntType::Unsigned(ast::UintTy::U32)))
    }

    pub fn expr_str(sp: Span, str: &str) -> P<ast::Expr> {
        self::expr_lit(sp, ast::LitKind::Str(Symbol::intern(str), ast::StrStyle::Cooked))
    }

    pub fn expr_tuple(sp: Span, exprs: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Tup(exprs))
    }

    pub fn expr_array(sp: Span, exprs: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Array(exprs))
    }

    pub fn expr_slice(sp: Span, exprs: Vec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr_ref(sp, self::expr_array(sp, exprs))
    }

    pub fn expr_cast(sp: Span, expr: P<ast::Expr>, ty: P<ast::Ty>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Cast(expr, ty))
    }

    pub fn block_check_mode(sp: Span, stmts: Vec<ast::Stmt>, block_check_mode: ast::BlockCheckMode) -> P<ast::Block> {
        P(ast::Block {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            rules: block_check_mode,
            stmts,
            tokens: None,
            could_be_bare_literal: false,
        })
    }

    pub fn block(sp: Span, stmts: Vec<ast::Stmt>) -> P<ast::Block> {
        self::block_check_mode(sp, stmts, ast::BlockCheckMode::Default)
    }

    pub fn block_unsafe(sp: Span, stmts: Vec<ast::Stmt>, unsafe_source: ast::UnsafeSource) -> P<ast::Block> {
        self::block_check_mode(sp, stmts, ast::BlockCheckMode::Unsafe(unsafe_source))
    }

    pub fn block_expr(expr: P<ast::Expr>) -> P<ast::Block> {
        self::block(expr.span, vec![self::stmt_expr(expr)])
    }

    pub fn vis(sp: Span, kind: ast::VisibilityKind) -> ast::Visibility {
        ast::Visibility { span: sp.shrink_to_lo(), kind, tokens: None }
    }

    pub fn vis_default(sp: Span) -> ast::Visibility {
        self::vis(sp, ast::VisibilityKind::Inherited)
    }

    pub fn vis_pub(sp: Span) -> ast::Visibility {
        self::vis(sp, ast::VisibilityKind::Public)
    }

    pub fn vis_pub_crate(sp: Span) -> ast::Visibility {
        self::vis(sp, ast::VisibilityKind::Crate(ast::CrateSugar::PubCrate))
    }

    pub fn item(sp: Span, attrs: Vec<ast::Attribute>, vis: ast::Visibility, ident: Ident, kind: ast::ItemKind) -> P<ast::Item> {
        P(ast::Item {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            attrs,
            vis,
            ident,
            kind,
            tokens: None,
        })
    }

    pub fn item_static(sp: Span, vis: ast::Visibility, mutbl: ast::Mutability, ident: Ident, ty: P<ast::Ty>, expr: P<ast::Expr>) -> P<ast::Item> {
        self::item(sp, vec![], vis, ident, ast::ItemKind::Static(ty, mutbl, Some(expr)))
    }

    pub fn item_const(sp: Span, vis: ast::Visibility, ident: Ident, ty: P<ast::Ty>, expr: P<ast::Expr>) -> P<ast::Item> {
        self::item(sp, vec![], vis, ident, ast::ItemKind::Const(ast::Defaultness::Final, ty, Some(expr)))
    }

    pub fn item_mod(sp: Span, vis: ast::Visibility, ident: Ident, items: Vec<P<ast::Item>>) -> P<ast::Item> {
        let mod_kind = ast::ModKind::Loaded(items, ast::Inline::Yes, ast::ModSpans { inner_span: sp, inject_use_span: sp });
        self::item(sp, vec![], vis, ident, ast::ItemKind::Mod(ast::Unsafe::No, mod_kind))
    }

    pub fn item_extern_crate(sp: Span, krate: Symbol, ident: Option<Ident>) -> P<ast::Item> {
        match ident {
            Some(ident) => self::item(sp, vec![], self::vis_default(sp), ident, ast::ItemKind::ExternCrate(Some(krate))),
            None => self::item(sp, vec![], self::vis_default(sp), Ident::new(krate, sp), ast::ItemKind::ExternCrate(None)),
        }
    }

    pub fn item_fn(sp: Span, vis: ast::Visibility, ident: Ident, generics: Option<ast::Generics>, header: Option<ast::FnHeader>, inputs: Vec<ast::Param>, output: Option<P<ast::Ty>>, body: Option<P<ast::Block>>) -> P<ast::Item> {
        self::item(sp, vec![], vis, ident, ast::ItemKind::Fn(Box::new(ast::Fn {
            defaultness: ast::Defaultness::Final,
            generics: generics.unwrap_or_default(),
            sig: ast::FnSig {
                span: sp,
                header: header.unwrap_or_default(),
                decl: self::fn_decl(inputs, match output {
                    Some(ty) => ast::FnRetTy::Ty(ty),
                    None => ast::FnRetTy::Default(sp),
                }),
            },
            body,
        })))
    }

    pub fn field_def(sp: Span, vis: ast::Visibility, ident: Option<Ident>, ty: P<ast::Ty>) -> ast::FieldDef {
        ast::FieldDef {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            attrs: ast::AttrVec::new(),
            vis,
            ident,
            ty,
            is_placeholder: false,
        }
    }

    pub fn item_struct(sp: Span, vis: ast::Visibility, ident: Ident, generics: Option<ast::Generics>, fields: Vec<ast::FieldDef>) -> P<ast::Item> {
        self::item(sp, vec![], vis, ident, ast::ItemKind::Struct(
            ast::VariantData::Struct(fields, false),
            generics.unwrap_or_default(),
        ))
    }

    pub fn item_tuple_struct(sp: Span, vis: ast::Visibility, ident: Ident, generics: Option<ast::Generics>, fields: Vec<ast::FieldDef>) -> P<ast::Item> {
        self::item(sp, vec![], vis, ident, ast::ItemKind::Struct(
            ast::VariantData::Tuple(fields, ast::DUMMY_NODE_ID),
            generics.unwrap_or_default(),
        ))
    }

    pub fn item_unit_struct(sp: Span, vis: ast::Visibility, ident: Ident) -> P<ast::Item> {
        self::item(sp, vec![], vis, ident, ast::ItemKind::Struct(
            ast::VariantData::Unit(ast::DUMMY_NODE_ID),
            Default::default(),
        ))
    }

    pub fn stmt(sp: Span, kind: ast::StmtKind) -> ast::Stmt {
        ast::Stmt { id: ast::DUMMY_NODE_ID, span: sp, kind }
    }

    pub fn stmt_expr(expr: P<ast::Expr>) -> ast::Stmt {
        self::stmt(expr.span, ast::StmtKind::Expr(expr))
    }

    pub fn stmt_local(sp: Span, mutbl: bool, ident: Ident, ty: Option<P<ast::Ty>>, kind: ast::LocalKind) -> ast::Stmt {
        let pat = match mutbl {
            true => self::pat_ident_binding_mode(sp, ident, ast::BindingMode::ByValue(ast::Mutability::Mut)),
            false => self::pat_ident(sp, ident),
        };

        self::stmt(sp, ast::StmtKind::Local(P(ast::Local {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            attrs: ast::AttrVec::new(),
            pat,
            ty,
            kind,
            tokens: None,
        })))
    }

    pub fn stmt_let_decl(sp: Span, mutbl: bool, ident: Ident, ty: Option<P<ast::Ty>>) -> ast::Stmt {
        self::stmt_local(sp, mutbl, ident, ty, ast::LocalKind::Decl)
    }

    pub fn stmt_let(sp: Span, mutbl: bool, ident: Ident, ty: Option<P<ast::Ty>>, expr: P<ast::Expr>) -> ast::Stmt {
        self::stmt_local(sp, mutbl, ident, ty, ast::LocalKind::Init(expr))
    }

    pub fn stmt_let_else(sp: Span, mutbl: bool, ident: Ident, ty: Option<P<ast::Ty>>, expr: P<ast::Expr>, els: P<ast::Block>) -> ast::Stmt {
        self::stmt_local(sp, mutbl, ident, ty, ast::LocalKind::InitElse(expr, els))
    }

    pub fn stmt_item(sp: Span, item: P<ast::Item>) -> ast::Stmt {
        self::stmt(sp, ast::StmtKind::Item(item))
    }

    pub fn token(sp: Span, kind: ast::token::TokenKind) -> ast::token::Token {
        ast::token::Token { span: sp, kind }
    }

    pub fn tt_token(sp: Span, kind: ast::token::TokenKind) -> ast::tokenstream::TokenTree {
        ast::tokenstream::TokenTree::Token(self::token(sp, kind))
    }

    pub fn ts_token(sp: Span, spacing: ast::tokenstream::Spacing, kind: ast::token::TokenKind) -> ast::tokenstream::TreeAndSpacing {
        (self::tt_token(sp, kind), spacing)
    }

    pub fn ts_path(sp: Span, mut path: ast::Path) -> Vec<ast::tokenstream::TreeAndSpacing> {
        assert!(path.segments.last().is_some_and(|s| s.args.is_none()));

        let path_sep_token = |sp: Span| self::ts_token(sp, ast::tokenstream::Spacing::Joint, ast::token::TokenKind::ModSep);
        let segment_token = |sp: Span, segment: ast::PathSegment| self::ts_token(sp, ast::tokenstream::Spacing::Joint, ast::token::TokenKind::Ident(segment.ident.name, false));

        let is_global = !path.segments[0].ident.is_path_segment_keyword();
        let mut tokens = Vec::with_capacity(2 * path.segments.len() - 1 + is_global as usize);

        let first_segment = path.segments.remove(0);
        if is_global { tokens.push(path_sep_token(sp)); }
        tokens.push(segment_token(sp, first_segment));

        for segment in path.segments {
            tokens.push(path_sep_token(sp));
            tokens.push(segment_token(sp, segment));
        }

        tokens
    }

    pub fn token_stream(token_trees: Vec<ast::tokenstream::TreeAndSpacing>) -> ast::tokenstream::TokenStream {
        ast::tokenstream::TokenStream::new(token_trees)
    }
}
