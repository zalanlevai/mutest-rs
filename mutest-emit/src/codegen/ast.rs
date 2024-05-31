pub use rustc_ast::*;
pub use rustc_ast::ptr::P;

pub use rustc_ast::token::TokenKind;
pub use rustc_ast::tokenstream::*;

use rustc_span::Span;
use rustc_span::symbol::Ident;

use crate::analysis::Descr;

#[derive(Clone, Debug)]
pub struct FnItem {
    pub id: ast::NodeId,
    pub span: Span,
    pub ctx: visit::FnCtxt,
    pub vis: ast::Visibility,
    pub ident: Ident,
    pub generics: ast::Generics,
    pub sig: ast::FnSig,
    pub body: Option<ast::Block>,
}

#[derive(Copy, Clone, Debug)]
pub enum DefItem<'ast> {
    Item(&'ast ast::Item),
    ForeignItem(&'ast ast::ForeignItem),
    AssocItem(&'ast ast::AssocItem, visit::AssocCtxt),
}

impl<'ast> DefItem<'ast> {
    pub fn ident(&self) -> Ident {
        match self {
            Self::Item(item) => item.ident,
            Self::ForeignItem(item) => item.ident,
            Self::AssocItem(item, _) => item.ident,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Item(item) => item.span,
            Self::ForeignItem(item) => item.span,
            Self::AssocItem(item, _) => item.span,
        }
    }

    pub fn kind(&self) -> ast::ItemKind {
        match self {
            Self::Item(item) => item.kind.clone(),
            Self::ForeignItem(item) => item.kind.clone().into(),
            Self::AssocItem(item, _) => item.kind.clone().into(),
        }
    }
}

pub mod mk {
    use rustc_ast as ast;
    use rustc_ast::ptr::P;
    use rustc_span::{Span, Symbol, sym};
    use rustc_span::symbol::{Ident, kw};
    use thin_vec::{ThinVec, thin_vec};

    pub fn angle_bracketed_args(sp: Span, args: Vec<ast::GenericArg>) -> Option<P<ast::GenericArgs>> {
        if args.is_empty() { return None; }

        let args = args.into_iter().map(ast::AngleBracketedArg::Arg).collect();
        Some(P(ast::GenericArgs::AngleBracketed(ast::AngleBracketedArgs { span: sp, args })))
    }

    pub fn parenthesized_args(sp: Span, inputs: ThinVec<P<ast::Ty>>, output: Option<P<ast::Ty>>) -> P<ast::GenericArgs> {
        P(ast::GenericArgs::Parenthesized(ast::ParenthesizedArgs {
            span: sp,
            inputs,
            inputs_span: sp,
            output: self::fn_ret_ty(sp, output),
        }))
    }

    pub fn path_segment_raw(sp: Span, ident: Ident, args: Option<P<ast::GenericArgs>>) -> ast::PathSegment {
        ast::PathSegment {
            id: ast::DUMMY_NODE_ID,
            ident: ident.with_span_pos(sp),
            args,
        }
    }

    pub fn path_segment(sp: Span, ident: Ident, args: Vec<ast::GenericArg>) -> ast::PathSegment {
        self::path_segment_raw(sp, ident, self::angle_bracketed_args(sp, args))
    }

    pub fn path_raw(sp: Span, global: bool, mut idents: Vec<Ident>, args: Option<P<ast::GenericArgs>>) -> ast::Path {
        assert!(!idents.is_empty());

        let add_root = global && !idents[0].is_path_segment_keyword();
        let mut segments = ThinVec::with_capacity(idents.len() + add_root as usize);
        if add_root {
            // NOTE: A path segment with an empty identifier is used instead of `ast::PathSegment::path_root`, because
            //       `ast::PathSegment::path_root` uses the `{{root}}` symbol which is improperly printed in token
            //       stream positions.
            segments.push(ast::PathSegment::from_ident(Ident::empty()));
        }

        let last_ident = idents.pop().unwrap();

        segments.extend(idents.into_iter().map(|ident| ast::PathSegment::from_ident(ident.with_span_pos(sp))));

        segments.push(self::path_segment_raw(sp, last_ident, args));

        ast::Path { span: sp, segments, tokens: None }
    }

    pub fn path_args(sp: Span, global: bool, idents: Vec<Ident>, args: Vec<ast::GenericArg>) -> ast::Path {
        self::path_raw(sp, global, idents, self::angle_bracketed_args(sp, args))
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

    pub fn pathx_raw(sp: Span, path: ast::Path, idents: Vec<Ident>, args: Option<P<ast::GenericArgs>>) -> ast::Path {
        let idents = path.segments.iter()
            .map(|s| s.ident)
            .chain(idents.into_iter())
            .collect::<Vec<_>>();

        self::path_raw(sp, false, idents, args)
    }

    pub fn pathx_args(sp: Span, path: ast::Path, idents: Vec<Ident>, args: Vec<ast::GenericArg>) -> ast::Path {
        self::pathx_raw(sp, path, idents, self::angle_bracketed_args(sp, args))
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

    pub fn ty_path(q_self: Option<P<ast::QSelf>>, path: ast::Path) -> P<ast::Ty> {
        self::ty(path.span, ast::TyKind::Path(q_self, path))
    }

    pub fn ty_ident(sp: Span, q_self: Option<P<ast::QSelf>>, ident: Ident) -> P<ast::Ty> {
        self::ty_path(q_self, self::path_ident(sp, ident))
    }

    pub fn ty_rptr(sp: Span, ty: P<ast::Ty>, lifetime: Option<ast::Lifetime>, mutbl: ast::Mutability) -> P<ast::Ty> {
        self::ty(sp, ast::TyKind::Ref(lifetime, self::ty_mut(ty, mutbl)))
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

    pub fn ty_tuple(sp: Span, tys: ThinVec<P<ast::Ty>>) -> P<ast::Ty> {
        self::ty(sp, ast::TyKind::Tup(tys))
    }

    pub fn ty_param(sp: Span, ident: Ident, bounds: ast::GenericBounds, default: Option<P<ast::Ty>>) -> ast::GenericParam {
        ast::GenericParam {
            id: ast::DUMMY_NODE_ID,
            attrs: ast::AttrVec::new(),
            ident: ident.with_span_pos(sp),
            bounds,
            kind: ast::GenericParamKind::Type { default },
            is_placeholder: false,
            colon_span: Some(sp),
        }
    }

    pub fn lifetime(sp: Span, ident: Ident) -> ast::Lifetime {
        ast::Lifetime { id: ast::DUMMY_NODE_ID, ident: ident.with_span_pos(sp) }
    }

    pub fn trait_ref(path: ast::Path) -> ast::TraitRef {
        ast::TraitRef { ref_id: ast::DUMMY_NODE_ID, path }
    }

    pub fn poly_trait_ref(sp: Span, path: ast::Path) -> ast::PolyTraitRef {
        ast::PolyTraitRef {
            span: sp,
            bound_generic_params: ThinVec::new(),
            trait_ref: self::trait_ref(path),
        }
    }

    pub fn trait_bound(modifiers: ast::TraitBoundModifiers, path: ast::Path) -> ast::GenericBound {
        ast::GenericBound::Trait(self::poly_trait_ref(path.span, path), modifiers)
    }

    pub fn lifetime_bound(lifetime: ast::Lifetime) -> ast::GenericBound {
        ast::GenericBound::Outlives(lifetime)
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

    pub fn expr_range(sp: Span, start: Option<P<ast::Expr>>, end: Option<P<ast::Expr>>, limits: ast::RangeLimits) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Range(start, end, limits))
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
        self::pat_ident_binding_mode(sp, ident, ast::BindingMode::NONE)
    }

    pub fn pat_path(sp: Span, path: ast::Path) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Path(None, path))
    }

    pub fn pat_tuple(sp: Span, pats: ThinVec<P<ast::Pat>>) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Tuple(pats))
    }

    pub fn pat_tuple_struct(sp: Span, path: ast::Path, pats:ThinVec<P<ast::Pat>>) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::TupleStruct(None, path, pats))
    }

    pub fn pat_struct(sp: Span, path: ast::Path, field_pats: ThinVec<ast::PatField>, rest: ast::PatFieldsRest) -> P<ast::Pat> {
        self::pat(sp, ast::PatKind::Struct(None, path, field_pats, rest))
    }

    pub fn arm(sp: Span, pat: P<ast::Pat>, guard: Option<P<ast::Expr>>, expr: Option<P<ast::Expr>>) -> ast::Arm {
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

    pub fn expr_match(span: Span, expr: P<ast::Expr>, arms: ThinVec<ast::Arm>) -> P<ast::Expr> {
        self::expr(span, ast::ExprKind::Match(expr, arms, ast::MatchKind::Prefix))
    }

    pub fn expr_if(sp: Span, cond: P<ast::Expr>, then: P<ast::Block>, els: Option<P<ast::Block>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::If(cond, then, els.map(self::expr_block)))
    }

    pub fn expr_call(sp: Span, expr: P<ast::Expr>, args: ThinVec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Call(expr, args))
    }

    pub fn expr_call_ident(sp: Span, ident: Ident, args: ThinVec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Call(self::expr_ident(sp, ident), args))
    }

    pub fn expr_call_path(sp: Span, path: ast::Path, args: ThinVec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Call(self::expr_path(path), args))
    }

    pub fn expr_method_call(sp: Span, receiver: P<ast::Expr>, path: ast::PathSegment, args: ThinVec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::MethodCall(Box::new(ast::MethodCall {
            seg: path,
            receiver,
            args,
            span: sp,
        })))
    }

    pub fn expr_method_call_path_ident(sp: Span, receiver: ast::Path, ident: Ident, args: ThinVec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr_method_call(sp, self::expr_path(receiver), self::path_segment(sp, ident, vec![]), args)
    }

    pub fn expr_field(sp: Span, expr: P<ast::Expr>, ident: Ident) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Field(expr, ident))
    }

    pub fn expr_field_deep(sp: Span, expr: P<ast::Expr>, idents: Vec<Ident>) -> P<ast::Expr> {
        idents.into_iter().fold(expr, |expr, ident| self::expr_field(sp, expr, ident))
    }

    pub fn expr_index(sp: Span, expr: P<ast::Expr>, index: P<ast::Expr>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Index(expr, index, sp))
    }

    pub fn expr_block(block: P<ast::Block>) -> P<ast::Expr> {
        self::expr(block.span, ast::ExprKind::Block(block, None))
    }

    pub fn expr_noop(sp: Span) -> P<ast::Expr> {
        self::expr_block(self::block(sp, ThinVec::new()))
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

    pub fn fn_ret_ty(sp: Span, ty: Option<P<ast::Ty>>) -> ast::FnRetTy {
        match ty {
            Some(ty) => ast::FnRetTy::Ty(ty),
            None => ast::FnRetTy::Default(sp),
        }
    }

    pub fn fn_decl(inputs: ThinVec<ast::Param>, output: ast::FnRetTy) -> P<ast::FnDecl> {
        P(ast::FnDecl { inputs, output })
    }

    pub fn expr_closure(sp: Span, idents: Vec<Ident>, body: P<ast::Expr>) -> P<ast::Expr> {
        let fn_decl = self::fn_decl(
            idents.into_iter()
                .map(|ident| self::param(sp, self::pat_ident(sp, ident), self::ty(sp, ast::TyKind::Infer)))
                .collect(),
            ast::FnRetTy::Default(sp),
        );

        self::expr(sp, ast::ExprKind::Closure(Box::new(ast::Closure {
            binder: ast::ClosureBinder::NotPresent,
            capture_clause: ast::CaptureBy::Ref,
            constness: ast::Const::No,
            coroutine_kind: None,
            movability: ast::Movability::Movable,
            fn_decl,
            body,
            fn_decl_span: sp,
            fn_arg_span: sp,
        })))
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

    pub fn expr_struct(sp: Span, path: ast::Path, fields: ThinVec<ast::ExprField>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Struct(P(ast::StructExpr {
            qself: None,
            path,
            fields,
            rest: ast::StructRest::None,
        })))
    }

    pub fn expr_struct_ident(sp: Span, ident: Ident, fields: ThinVec<ast::ExprField>) -> P<ast::Expr> {
        self::expr_struct(sp, self::path_ident(sp, ident), fields)
    }

    pub fn expr_lit(sp: Span, kind: ast::token::LitKind, symbol: Symbol, suffix: Option<Symbol>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Lit(ast::token::Lit::new(kind, symbol, suffix)))
    }

    pub fn expr_bool(sp: Span, value: bool) -> P<ast::Expr> {
        let symbol = match value {
            true => kw::True,
            false => kw::False,
        };

        self::expr_lit(sp, ast::token::LitKind::Bool, symbol, None)
    }

    pub fn expr_int(sp: Span, i: isize) -> P<ast::Expr> {
        let abs_symbol = Symbol::intern(&i.abs().to_string());
        let abs = self::expr_lit(sp, ast::token::LitKind::Integer, abs_symbol, None);

        match i {
            0.. => abs,
            _ => self::expr_unary(sp, ast::UnOp::Neg, abs)
        }
    }

    pub fn expr_int_exact(sp: Span, i: isize, suffix: Symbol) -> P<ast::Expr> {
        let abs_symbol = Symbol::intern(&i.abs().to_string());
        let abs = self::expr_lit(sp, ast::token::LitKind::Integer, abs_symbol, Some(suffix));

        match i {
            0.. => abs,
            _ => self::expr_unary(sp, ast::UnOp::Neg, abs)
        }
    }

    pub fn expr_float_exact(sp: Span, v: f64, suffix: Symbol) -> P<ast::Expr> {
        let abs_symbol = Symbol::intern(&v.abs().to_string());
        let abs = self::expr_lit(sp, ast::token::LitKind::Float, abs_symbol, Some(suffix));

        match v {
            0_f64.. => abs,
            _ => self::expr_unary(sp, ast::UnOp::Neg, abs)
        }
    }

    pub fn expr_usize(sp: Span, i: usize) -> P<ast::Expr> {
        self::expr_lit(sp, ast::token::LitKind::Integer, Symbol::intern(&i.to_string()), Some(sym::usize))
    }

    pub fn expr_u32(sp: Span, i: u32) -> P<ast::Expr> {
        self::expr_lit(sp, ast::token::LitKind::Integer, Symbol::intern(&i.to_string()), Some(sym::u32))
    }

    pub fn expr_str(sp: Span, str: &str) -> P<ast::Expr> {
        self::expr_lit(sp, ast::token::LitKind::Str, Symbol::intern(str), None)
    }

    pub fn expr_tuple(sp: Span, exprs: ThinVec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Tup(exprs))
    }

    pub fn expr_array(sp: Span, exprs: ThinVec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Array(exprs))
    }

    pub fn expr_slice(sp: Span, exprs: ThinVec<P<ast::Expr>>) -> P<ast::Expr> {
        self::expr_ref(sp, self::expr_array(sp, exprs))
    }

    pub fn expr_cast(sp: Span, expr: P<ast::Expr>, ty: P<ast::Ty>) -> P<ast::Expr> {
        self::expr(sp, ast::ExprKind::Cast(expr, ty))
    }

    pub fn block_check_mode(sp: Span, stmts: ThinVec<ast::Stmt>, block_check_mode: ast::BlockCheckMode) -> P<ast::Block> {
        P(ast::Block {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            rules: block_check_mode,
            stmts,
            tokens: None,
            could_be_bare_literal: false,
        })
    }

    pub fn block(sp: Span, stmts: ThinVec<ast::Stmt>) -> P<ast::Block> {
        self::block_check_mode(sp, stmts, ast::BlockCheckMode::Default)
    }

    pub fn block_unsafe(sp: Span, stmts: ThinVec<ast::Stmt>, unsafe_source: ast::UnsafeSource) -> P<ast::Block> {
        self::block_check_mode(sp, stmts, ast::BlockCheckMode::Unsafe(unsafe_source))
    }

    pub fn block_expr(expr: P<ast::Expr>) -> P<ast::Block> {
        self::block(expr.span, thin_vec![self::stmt_expr(expr)])
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
        self::vis(sp, ast::VisibilityKind::Restricted {
            id: ast::DUMMY_NODE_ID,
            path: P(self::path_ident(sp, Ident::new(kw::Crate, sp))),
            shorthand: true,
        })
    }

    pub fn item(sp: Span, attrs: ThinVec<ast::Attribute>, vis: ast::Visibility, ident: Ident, kind: ast::ItemKind) -> P<ast::Item> {
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
        self::item(sp, ThinVec::new(), vis, ident, ast::ItemKind::Static(Box::new(ast::StaticItem {
            ty,
            mutability: mutbl,
            expr: Some(expr),
        })))
    }

    pub fn item_const(sp: Span, vis: ast::Visibility, ident: Ident, ty: P<ast::Ty>, expr: P<ast::Expr>) -> P<ast::Item> {
        self::item(sp, ThinVec::new(), vis, ident, ast::ItemKind::Const(Box::new(ast::ConstItem {
            defaultness: ast::Defaultness::Final,
            generics: ast::Generics {
                params: ThinVec::new(),
                where_clause: ast::WhereClause {
                    has_where_token: false,
                    predicates: ThinVec::new(),
                    span: sp,
                },
                span: sp,
            },
            ty,
            expr: Some(expr),
        })))
    }

    pub fn item_mod(sp: Span, vis: ast::Visibility, ident: Ident, items: ThinVec<P<ast::Item>>) -> P<ast::Item> {
        let mod_kind = ast::ModKind::Loaded(items, ast::Inline::Yes, ast::ModSpans { inner_span: sp, inject_use_span: sp });
        self::item(sp, ThinVec::new(), vis, ident, ast::ItemKind::Mod(ast::Unsafe::No, mod_kind))
    }

    pub fn item_extern_crate(sp: Span, krate: Symbol, ident: Option<Ident>) -> P<ast::Item> {
        match ident {
            Some(ident) => self::item(sp, ThinVec::new(), self::vis_default(sp), ident, ast::ItemKind::ExternCrate(Some(krate))),
            None => self::item(sp, ThinVec::new(), self::vis_default(sp), Ident::new(krate, sp), ast::ItemKind::ExternCrate(None)),
        }
    }

    pub fn item_fn(sp: Span, vis: ast::Visibility, ident: Ident, generics: Option<ast::Generics>, header: Option<ast::FnHeader>, inputs: ThinVec<ast::Param>, output: Option<P<ast::Ty>>, body: Option<P<ast::Block>>) -> P<ast::Item> {
        self::item(sp, ThinVec::new(), vis, ident, ast::ItemKind::Fn(Box::new(ast::Fn {
            defaultness: ast::Defaultness::Final,
            generics: generics.unwrap_or_default(),
            sig: ast::FnSig {
                span: sp,
                header: header.unwrap_or_default(),
                decl: self::fn_decl(inputs, self::fn_ret_ty(sp, output)),
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

    pub fn item_struct(sp: Span, vis: ast::Visibility, ident: Ident, generics: Option<ast::Generics>, fields: ThinVec<ast::FieldDef>) -> P<ast::Item> {
        self::item(sp, ThinVec::new(), vis, ident, ast::ItemKind::Struct(
            ast::VariantData::Struct { fields, recovered: ast::Recovered::No },
            generics.unwrap_or_default(),
        ))
    }

    pub fn item_tuple_struct(sp: Span, vis: ast::Visibility, ident: Ident, generics: Option<ast::Generics>, fields: ThinVec<ast::FieldDef>) -> P<ast::Item> {
        self::item(sp, ThinVec::new(), vis, ident, ast::ItemKind::Struct(
            ast::VariantData::Tuple(fields, ast::DUMMY_NODE_ID),
            generics.unwrap_or_default(),
        ))
    }

    pub fn item_unit_struct(sp: Span, vis: ast::Visibility, ident: Ident) -> P<ast::Item> {
        self::item(sp, ThinVec::new(), vis, ident, ast::ItemKind::Struct(
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
            true => self::pat_ident_binding_mode(sp, ident, ast::BindingMode::MUT),
            false => self::pat_ident(sp, ident),
        };

        let has_ty = ty.is_some();

        self::stmt(sp, ast::StmtKind::Let(P(ast::Local {
            id: ast::DUMMY_NODE_ID,
            span: sp,
            attrs: ast::AttrVec::new(),
            pat,
            ty,
            kind,
            colon_sp: has_ty.then_some(sp),
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

    pub fn attr_inner_path(g: &ast::attr::AttrIdGenerator, sp: Span, path: ast::Path, args: ast::AttrArgs) -> ast::Attribute {
        ast::attr::mk_attr(g, ast::AttrStyle::Inner, path, args, sp)
    }

    pub fn attr_outer_path(g: &ast::attr::AttrIdGenerator, sp: Span, path: ast::Path, args: ast::AttrArgs) -> ast::Attribute {
        ast::attr::mk_attr(g, ast::AttrStyle::Outer, path, args, sp)
    }

    pub fn attr_inner(g: &ast::attr::AttrIdGenerator, sp: Span, ident: Ident, args: ast::AttrArgs) -> ast::Attribute {
        ast::attr::mk_attr(g, ast::AttrStyle::Inner, ast::Path::from_ident(ident), args, sp)
    }

    pub fn attr_outer(g: &ast::attr::AttrIdGenerator, sp: Span, ident: Ident, args: ast::AttrArgs) -> ast::Attribute {
        ast::attr::mk_attr(g, ast::AttrStyle::Outer, ast::Path::from_ident(ident), args, sp)
    }

    pub fn attr_args_delimited(sp: Span, delimiter: ast::token::Delimiter, tokens: ast::tokenstream::TokenStream) -> ast::AttrArgs {
        ast::AttrArgs::Delimited(ast::DelimArgs {
            dspan: ast::tokenstream::DelimSpan::from_single(sp),
            delim: delimiter,
            tokens,
        })
    }

    pub fn token(sp: Span, kind: ast::token::TokenKind) -> ast::token::Token {
        ast::token::Token { span: sp, kind }
    }

    pub fn tt_token_alone(sp: Span, kind: ast::token::TokenKind) -> ast::tokenstream::TokenTree {
        ast::tokenstream::TokenTree::Token(self::token(sp, kind), ast::tokenstream::Spacing::Alone)
    }

    pub fn tt_token_joint(sp: Span, kind: ast::token::TokenKind) -> ast::tokenstream::TokenTree {
        ast::tokenstream::TokenTree::Token(self::token(sp, kind), ast::tokenstream::Spacing::Joint)
    }

    pub fn tt_token_joint_hidden(sp: Span, kind: ast::token::TokenKind) -> ast::tokenstream::TokenTree {
        ast::tokenstream::TokenTree::Token(self::token(sp, kind), ast::tokenstream::Spacing::JointHidden)
    }

    pub fn tt_delimited(sp: Span, delimiter: ast::token::Delimiter, token_stream: ast::tokenstream::TokenStream) -> ast::tokenstream::TokenTree {
        let delim_span = ast::tokenstream::DelimSpan::from_single(sp);
        let delim_spacing = ast::tokenstream::DelimSpacing::new(ast::tokenstream::Spacing::Joint, ast::tokenstream::Spacing::Alone);
        ast::tokenstream::TokenTree::Delimited(delim_span, delim_spacing, delimiter, token_stream)
    }

    pub fn ts_path(sp: Span, mut path: ast::Path) -> Vec<ast::tokenstream::TokenTree> {
        assert!(path.segments.last().is_some_and(|s| s.args.is_none()));

        let path_sep_token = |sp: Span| self::tt_token_joint(sp, ast::token::TokenKind::PathSep);
        let segment_token = |sp: Span, segment: ast::PathSegment| self::tt_token_joint_hidden(sp, ast::token::TokenKind::Ident(segment.ident.name, ast::token::IdentIsRaw::No));

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

    pub fn token_stream(token_trees: Vec<ast::tokenstream::TokenTree>) -> ast::tokenstream::TokenStream {
        ast::tokenstream::TokenStream::new(token_trees)
    }
}

impl Descr for ast::StmtKind {
    fn descr(&self) -> &'static str {
        match self {
            ast::StmtKind::Item(..) => "item",
            ast::StmtKind::Let(..) => "let",
            ast::StmtKind::Semi(..) => "statement expression",
            ast::StmtKind::Expr(..) => "trailing expression",
            ast::StmtKind::MacCall(..) => "macro call",
            ast::StmtKind::Empty => "empty",
        }
    }
}

impl Descr for ast::ExprKind {
    fn descr(&self) -> &'static str {
        match self {
            ast::ExprKind::Array(..) => "array literal",
            ast::ExprKind::ConstBlock(..) => "const block",
            ast::ExprKind::Call(..) => "call",
            ast::ExprKind::MethodCall(..) => "method call",
            ast::ExprKind::Tup(..) => "tuple literal",
            ast::ExprKind::Binary(..) => "binary operation",
            ast::ExprKind::Unary(..) => "unary operation",
            ast::ExprKind::Lit(..) => "literal",
            ast::ExprKind::Cast(..) => "cast",
            ast::ExprKind::Type(..) => "type ascription",
            ast::ExprKind::Let(..) => "let",
            ast::ExprKind::If(..) => "if",
            ast::ExprKind::While(..) => "while",
            ast::ExprKind::ForLoop { .. } => "for loop",
            ast::ExprKind::Loop(..) => "loop",
            ast::ExprKind::Match(..) => "match",
            ast::ExprKind::Closure(..) => "closure",
            ast::ExprKind::Block(..) => "block",
            ast::ExprKind::Gen(.., ast::GenBlockKind::Async) => "async block",
            ast::ExprKind::Gen(.., ast::GenBlockKind::Gen) => "generator block",
            ast::ExprKind::Gen(.., ast::GenBlockKind::AsyncGen) => "async generator block",
            ast::ExprKind::Await(..) => "await",
            ast::ExprKind::TryBlock(..) => "try block",
            ast::ExprKind::Assign(..) => "assignment",
            ast::ExprKind::AssignOp(..) => "assignment with operator",
            ast::ExprKind::Field(..) => "field access",
            ast::ExprKind::Index(..) => "index",
            ast::ExprKind::Range(..) => "range",
            ast::ExprKind::Underscore => "_",
            ast::ExprKind::Path(..) => "path",
            ast::ExprKind::AddrOf(..) => "reference",
            ast::ExprKind::Break(..) => "break",
            ast::ExprKind::Continue(..) => "continue",
            ast::ExprKind::Ret(..) => "return",
            ast::ExprKind::InlineAsm(..) => "inline assembly",
            ast::ExprKind::OffsetOf(..) => "field offset",
            ast::ExprKind::MacCall(..) => "macro call",
            ast::ExprKind::Struct(..) => "struct literal",
            ast::ExprKind::Repeat(..) => "array from repetition",
            ast::ExprKind::Paren(..) => "parentheses",
            ast::ExprKind::Try(..) => "try",
            ast::ExprKind::Yield(..) => "yield",
            ast::ExprKind::Yeet(..) => "yeet",
            ast::ExprKind::Become(..) => "become",
            ast::ExprKind::IncludedBytes(..) => "included bytes",
            ast::ExprKind::FormatArgs(..) => "format_args",
            ast::ExprKind::Err(..) => "error",
            ast::ExprKind::Dummy => "dummy",
        }
    }
}

pub mod print {
    pub use rustc_ast_pretty::pprust::*;

    use rustc_ast as ast;

    pub fn stmt_to_string(stmt: &ast::Stmt) -> String {
        State::new().stmt_to_string(stmt)
    }
}

pub mod inspect {
    use std::iter;

    use rustc_ast as ast;
    use rustc_span::Symbol;

    pub fn match_attr_name(attr: &ast::Attribute, tool: Option<Symbol>, name: Symbol) -> bool {
        let ast::AttrKind::Normal(attr_item) = &attr.kind else { return false; };
        match (tool, &attr_item.item.path.segments[..]) {
            (None, [path_name]) => path_name.ident.name == name,
            (Some(tool), [path_tool, path_name]) => path_tool.ident.name == tool && path_name.ident.name == name,
            _ => false,
        }
    }

    pub fn is_word_attr(attr: &ast::Attribute, tool: Option<Symbol>, word: Symbol) -> bool {
        let Some(ast::MetaItemKind::Word) = attr.meta_kind() else { return false; };
        match_attr_name(attr, tool, word)
    }

    pub fn is_name_value_attr(attr: &ast::Attribute, tool: Option<Symbol>, name: Symbol, value: &ast::MetaItemLit) -> bool {
        let Some(ast::MetaItemKind::NameValue(lit)) = attr.meta_kind() else { return false; };
        match_attr_name(attr, tool, name) && lit.kind == value.kind
    }

    pub fn is_list_attr_with_path(attr: &ast::Attribute, tool: Option<Symbol>, name: Symbol, path: &ast::Path) -> bool {
        let Some(ast::MetaItemKind::List(meta_items)) = attr.meta_kind() else { return false; };
        match_attr_name(attr, tool, name) && meta_items.iter().any(|meta_item| {
            let Some(ast::MetaItem { path: meta_path, kind: ast::MetaItemKind::Word, .. }) = meta_item.meta_item() else { return false };
            iter::zip(&meta_path.segments, &path.segments).all(|(a, b)| a.ident.name == b.ident.name)
        })
    }

    pub fn is_list_attr_with_ident(attr: &ast::Attribute, tool: Option<Symbol>, name: Symbol, ident: Symbol) -> bool {
        let Some(ast::MetaItemKind::List(meta_items)) = attr.meta_kind() else { return false; };
        match_attr_name(attr, tool, name) && meta_items.iter().any(|meta_item| {
            let Some(ast::MetaItem { path: meta_path, kind: ast::MetaItemKind::Word, .. }) = meta_item.meta_item() else { return false };
            meta_path.segments.len() == 1 && meta_path.segments[0].ident.name == ident
        })
    }

    pub fn is_extern_crate_decl(item: &ast::Item, sym: Symbol) -> bool {
        if let ast::ItemKind::ExternCrate(..) = item.kind {
            if item.ident.name == sym {
                return true;
            }
        }

        false
    }
}

pub mod mut_visit {
    pub use rustc_ast::mut_visit::*;

    use rustc_ast::*;

    // Copy of `rustc_ast::mut_visit::noop_visit_vis`, which has been made private.
    pub fn noop_visit_vis<T: MutVisitor>(visibility: &mut Visibility, vis: &mut T) {
        match &mut visibility.kind {
            VisibilityKind::Public | VisibilityKind::Inherited => {}
            VisibilityKind::Restricted { path, id, shorthand: _ } => {
                vis.visit_path(path);
                vis.visit_id(id);
            }
        }
        vis.visit_span(&mut visibility.span);
    }

    // Copy of `rustc_ast::mut_visit::noop_visit_constraint`, which has been made private.
    pub fn noop_visit_constraint<T: MutVisitor>(assoc_constraint: &mut AssocConstraint, vis: &mut T) {
        vis.visit_id(&mut assoc_constraint.id);
        vis.visit_ident(&mut assoc_constraint.ident);
        if let Some(gen_args) = &mut assoc_constraint.gen_args { vis.visit_generic_args(gen_args); }
        match &mut assoc_constraint.kind {
            AssocConstraintKind::Equality { term } => match term {
                Term::Ty(ty) => vis.visit_ty(ty),
                Term::Const(c) => vis.visit_anon_const(c),
            }
            AssocConstraintKind::Bound { bounds } => {
                for bound in bounds {
                    vis.visit_param_bound(bound);
                }
            }
        }
        vis.visit_span(&mut assoc_constraint.span);
    }
}
