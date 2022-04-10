use rustc_ast as ast;
use rustc_ast::DUMMY_NODE_ID;
use rustc_ast::token::{Nonterminal, Token, TokenKind};
use rustc_ast::tokenstream::{DelimSpan, Spacing, TokenStream, TokenTree};
use rustc_data_structures::fx::FxHashMap;
use rustc_data_structures::thin_vec::ThinVec;
use rustc_hir as hir;
use rustc_middle::hir::map::Map;
use rustc_middle::ty::TyCtxt;
use rustc_span::{DUMMY_SP, Span, Symbol, symbol::Ident};
use smallvec::{SmallVec, smallvec};

pub struct AstNodeRef {
    pub id: ast::NodeId,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AnnotationId(Span);

const ANNOTATION_ENCODE_CONFIG: base64::Config = base64::Config::new(base64::CharacterSet::UrlSafe, false);

impl AnnotationId {
    pub fn new(span: Span) -> AnnotationId {
        Self(span)
    }

    pub fn span(&self) -> Span {
        self.0
    }

    pub fn to_symbol(&self) -> Symbol {
        let span_string = format!("{:?}", self.0);
        let encoded_span_string = base64::encode_config(span_string, ANNOTATION_ENCODE_CONFIG);

        Symbol::intern(&encoded_span_string)
    }
}

pub enum AnnotationKind {
    Expr(),
    Stmt(),
    Local(),
}

pub struct Annotation {
    pub id: AnnotationId,
    pub kind: AnnotationKind,
}

pub struct AnnotationCtxt<'ast> {
    pub source_ast: &'ast ast::Crate,
    // pub annotated_ast: ast::Crate,
    pub annotated_nodes: FxHashMap<ast::NodeId, Annotation>,
}

impl<'ast> AnnotationCtxt<'ast> {
    pub fn new(ast: &'ast ast::Crate) -> Self {
        Self {
            source_ast: ast,
            // annotated_ast: ast.clone(),
            annotated_nodes: FxHashMap::default(),
        }
    }

    pub fn annotate<'tcx>(&mut self, tcx: TyCtxt<'tcx>) -> ast::Crate {
        // AstAnnotationWriter { tcx, acx: self }.visit_crate(self.annotated_ast);

        println!("original_ast: {:#?}", self.source_ast);
        let mut annotated_ast = self.source_ast.clone();
        println!("cloned_unannotated_ast: {:#?}", annotated_ast);

        let mut writer = AstAnnotationWriter { tcx, acx: self };
        ast::mut_visit::MutVisitor::visit_crate(&mut writer, &mut annotated_ast);

        annotated_ast
    }
}

// pub struct HirAnnotator<'tcx> {
//     pub tcx: TyCtxt<'tcx>,
//     pub acx: &'tcx mut AnnotationCtxt<'tcx>,
// }
//
// impl<'tcx> hir::intravisit::Visitor<'tcx> for HirAnnotator<'tcx> {
//     type Map = Map<'tcx>;
//
//     fn nested_visit_map(&mut self) -> hir::intravisit::NestedVisitorMap<Self::Map> {
//         hir::intravisit::NestedVisitorMap::OnlyBodies(self.tcx.hir())
//     }
// }

fn mk_annotation<'acx, 'ast>(acx: &'acx mut AnnotationCtxt<'ast>, node_id: ast::NodeId, span: Span, kind: AnnotationKind) -> &'acx Annotation {
    let annotation = Annotation {
        id: AnnotationId::new(span),
        kind,
    };

    // if let Some(conflicting_annotation) = acx.annotated_nodes.insert(node_id, annotation) {
    //     panic!("conflicting annotations: node with id {} is already annotated with {:?}", node_id, conflicting_annotation.id);
    // }

    match acx.annotated_nodes.try_insert(node_id, annotation) {
        Ok(annotation) => annotation,
        Err(conflict) => panic!("conflicting annotations: node with id {} is already annotated with {:?}", node_id, conflict.entry.get().id),
    }
}

const RUNTIME_CRATE: &str = "mutest_runtime";

const LOCAL_MAC: &str = "local";

fn mk_local_mac_call<'tcx>(tcx: TyCtxt<'tcx>, annotation_id: Symbol, pat: ast::ptr::P<ast::Pat>) -> ast::MacCallStmt {
    let mac_path = ast::Path {
        span: DUMMY_SP,
        segments: vec![
            ast::PathSegment {
                id: DUMMY_NODE_ID,
                ident: Ident { name: Symbol::intern(RUNTIME_CRATE), span: DUMMY_SP },
                args: None,
            },
            ast::PathSegment {
                id: DUMMY_NODE_ID,
                ident: Ident { name: Symbol::intern(LOCAL_MAC), span: DUMMY_SP },
                args: None,
            },
        ],
        tokens: None,
    };

    let mac_args = ast::MacArgs::Delimited(
        DelimSpan::from_single(DUMMY_SP),
        ast::MacDelimiter::Parenthesis,
        TokenStream::from_streams(smallvec![
            TokenStream::new(vec![
                (TokenTree::Token(Token { span: DUMMY_SP, kind: TokenKind::Ident(annotation_id, true) }), Spacing::Alone),
                (TokenTree::Token(Token { span: DUMMY_SP, kind: TokenKind::Comma }), Spacing::Alone),
            ]),
            rustc_parse::fake_token_stream(&tcx.sess.parse_sess, &Nonterminal::NtPat(pat)),
        ]),
    );

    ast::MacCallStmt {
        mac: ast::MacCall {
            path: mac_path,
            args: ast::ptr::P(mac_args),
            prior_type_ascription: None,
        },
        style: ast::MacStmtStyle::Semicolon,
        attrs: ThinVec::new(),
        tokens: None,
    }
}

const EXPR_MAC: &str = "expr";

fn mk_expr_mac_call<'tcx>(tcx: TyCtxt<'tcx>, annotation_id: Symbol, expr: ast::ptr::P<ast::Expr>) -> ast::MacCall {
    let mac_path = ast::Path {
        span: DUMMY_SP,
        segments: vec![
            ast::PathSegment {
                id: DUMMY_NODE_ID,
                ident: Ident { name: Symbol::intern(RUNTIME_CRATE), span: DUMMY_SP },
                args: None,
            },
            ast::PathSegment {
                id: DUMMY_NODE_ID,
                ident: Ident { name: Symbol::intern(EXPR_MAC), span: DUMMY_SP },
                args: None,
            },
        ],
        tokens: None,
    };

    let mac_args = ast::MacArgs::Delimited(
        DelimSpan::from_single(DUMMY_SP),
        ast::MacDelimiter::Parenthesis,
        TokenStream::from_streams(smallvec![
            TokenStream::new(vec![
                (TokenTree::Token(Token { span: DUMMY_SP, kind: TokenKind::Ident(annotation_id, true) }), Spacing::Alone),
                (TokenTree::Token(Token { span: DUMMY_SP, kind: TokenKind::Comma }), Spacing::Alone),
            ]),
            rustc_parse::fake_token_stream(&tcx.sess.parse_sess, &Nonterminal::NtExpr(expr)),
        ]),
    );

    ast::MacCall {
        path: mac_path,
        args: ast::ptr::P(mac_args),
        prior_type_ascription: None,
    }
}

fn annotate_expr<'tcx, 'acx, 'ast>(tcx: TyCtxt<'tcx>, acx: &'acx mut AnnotationCtxt<'ast>, original_ref: AstNodeRef, expr: &mut ast::ptr::P<ast::Expr>) {
    println!("mk_annotation: {} {:#?} {:#?}", original_ref.id, original_ref.span, expr);
    let annotation = mk_annotation(acx, original_ref.id, original_ref.span, AnnotationKind::Expr());

    *expr = {
        let mac_call = mk_expr_mac_call(tcx, annotation.id.to_symbol(), expr.clone());
        let kind = ast::ExprKind::MacCall(mac_call);

        ast::ptr::P(ast::Expr { kind, ..expr.clone().into_inner() })
    };
}

const STMT_MAC: &str = "stmt";

fn mk_stmt_mac_call<'tcx>(tcx: TyCtxt<'tcx>, annotation_id: Symbol, expr: ast::ptr::P<ast::Expr>) -> ast::MacCallStmt {
    let mac_path = ast::Path {
        span: DUMMY_SP,
        segments: vec![
            ast::PathSegment {
                id: DUMMY_NODE_ID,
                ident: Ident { name: Symbol::intern(RUNTIME_CRATE), span: DUMMY_SP },
                args: None,
            },
            ast::PathSegment {
                id: DUMMY_NODE_ID,
                ident: Ident { name: Symbol::intern(STMT_MAC), span: DUMMY_SP },
                args: None,
            },
        ],
        tokens: None,
    };

    let mac_args = ast::MacArgs::Delimited(
        DelimSpan::from_single(DUMMY_SP),
        ast::MacDelimiter::Parenthesis,
        TokenStream::from_streams(smallvec![
            TokenStream::new(vec![
                (TokenTree::Token(Token { span: DUMMY_SP, kind: TokenKind::Ident(annotation_id, true) }), Spacing::Alone),
                (TokenTree::Token(Token { span: DUMMY_SP, kind: TokenKind::Comma }), Spacing::Alone),
            ]),
            rustc_parse::fake_token_stream(&tcx.sess.parse_sess, &Nonterminal::NtExpr(expr)),
        ]),
    );

    ast::MacCallStmt {
        mac: ast::MacCall {
            path: mac_path,
            args: ast::ptr::P(mac_args),
            prior_type_ascription: None,
        },
        style: ast::MacStmtStyle::Semicolon,
        attrs: ThinVec::new(),
        tokens: None,
    }
}

pub struct AstAnnotationWriter<'tcx, 'acx, 'ast> {
    pub tcx: TyCtxt<'tcx>,
    pub acx: &'acx mut AnnotationCtxt<'ast>,
}

impl<'tcx, 'acx, 'ast> ast::mut_visit::MutVisitor for AstAnnotationWriter<'tcx, 'acx, 'ast> {
    fn flat_map_stmt(&mut self, stmt: ast::Stmt) -> SmallVec<[ast::Stmt; 1]> {
        let annotation_id = AnnotationId::new(stmt.span).to_symbol();

        let stmt = {
            let kind = match stmt.kind {
                ast::StmtKind::Local(local) => {
                    let mac_call_stmt = mk_local_mac_call(self.tcx, annotation_id, local.pat.clone());
                    ast::StmtKind::MacCall(ast::ptr::P(mac_call_stmt))
                },
                ast::StmtKind::Expr(mut expr) => {
                    self.visit_expr(&mut expr);
                    ast::StmtKind::Expr(expr)
                },
                ast::StmtKind::Semi(mut expr) => {
                    self.visit_expr(&mut expr);

                    let mac_call_stmt = mk_stmt_mac_call(self.tcx, annotation_id, expr);
                    ast::StmtKind::MacCall(ast::ptr::P(mac_call_stmt))
                },

                // The following statements are kept as-is:
                // * `StmtKind::Item`: Nested items are not interesting, they only have to be recursed into.
                // * `StmtKind::MacCall`: Macros (both calls and definitions) are fragile, user and library-defined
                //   mechanisms. They are impossible to analyze predictably and thus should be avoided.
                // * `StmtKind::Empty`: Empty statements are not interesting, there is no point in annotating them.
                | ast::StmtKind::Item(_)
                | ast::StmtKind::MacCall(_)
                | ast::StmtKind::Empty
                => return ast::mut_visit::noop_flat_map_stmt(stmt, self),
            };

            ast::Stmt { kind, ..stmt }
        };

        smallvec![stmt]
    }

    fn visit_expr(&mut self, expr: &mut ast::ptr::P<ast::Expr>) {
        // let annotation_id = AnnotationId::new(expr.span).to_symbol();
        let original_ref = AstNodeRef { id: expr.id.clone(), span: expr.span.clone() };
        println!("AstNodeRef {} {:#?}", original_ref.id, original_ref.span);

        fn visit_subexprs<T: ast::mut_visit::MutVisitor>(expr: &mut ast::Expr, vis: &mut T) {
            // We want this match to break when `ast::ExprKind` changes so that we can
            // evaluate if subexpression visiting has to change accordingly. For this, all
            // `ast::ExprKind` variants are matched manually.
            match &mut expr.kind {
                // Most subexpressions can be visited using `ast::mut_visit::noop_visit_expr`.
                | ast::ExprKind::Box(_)
                | ast::ExprKind::Array(_)
                | ast::ExprKind::ConstBlock(_)
                | ast::ExprKind::Call(_, _)
                | ast::ExprKind::MethodCall(_, _, _)
                | ast::ExprKind::Tup(_)
                | ast::ExprKind::Binary(_, _, _)
                | ast::ExprKind::Unary(_, _)
                | ast::ExprKind::Lit(_)
                | ast::ExprKind::Cast(_, _)
                | ast::ExprKind::Type(_, _)
                | ast::ExprKind::Let(_, _, _)
                | ast::ExprKind::While(_, _, _)
                | ast::ExprKind::ForLoop(_, _, _, _)
                | ast::ExprKind::Loop(_, _)
                | ast::ExprKind::Match(_, _)
                | ast::ExprKind::Closure(_, _, _, _, _, _)
                | ast::ExprKind::Block(_, _)
                | ast::ExprKind::Async(_, _, _)
                | ast::ExprKind::Await(_)
                | ast::ExprKind::TryBlock(_)
                | ast::ExprKind::Assign(_, _, _)
                | ast::ExprKind::AssignOp(_, _, _)
                | ast::ExprKind::Field(_, _)
                | ast::ExprKind::Index(_, _)
                | ast::ExprKind::Range(_, _, _)
                | ast::ExprKind::Underscore
                | ast::ExprKind::Path(_, _)
                | ast::ExprKind::AddrOf(_, _, _)
                | ast::ExprKind::Break(_, _)
                | ast::ExprKind::Continue(_)
                | ast::ExprKind::Ret(_)
                | ast::ExprKind::InlineAsm(_)
                // | ast::ExprKind::LlvmInlineAsm(_)
                | ast::ExprKind::MacCall(_)
                | ast::ExprKind::Struct(_)
                | ast::ExprKind::Repeat(_, _)
                | ast::ExprKind::Paren(_)
                | ast::ExprKind::Try(_)
                | ast::ExprKind::Yield(_)
                | ast::ExprKind::Err
                => ast::mut_visit::noop_visit_expr(expr, vis),

                ast::ExprKind::If(cond, tr, fl) => {
                    vis.visit_expr(cond);
                    vis.visit_block(tr);

                    // The else branch of if expressions is only valid if it is a block or another
                    // if expression. As such, the else branch cannot be visited directly, as it
                    // would be turned into a macro call, which is invalid in that position.
                    // Instead, only the subexpressions of the else branch are visited.
                    ast::mut_visit::visit_opt(fl, |fl| visit_subexprs(fl, vis));
                },
            }
        }

        visit_subexprs(expr, self);

        // *expr = {
        //     let mac_call = mk_expr_mac_call(self.tcx, annotation_id, expr.clone());
        //     let kind = ast::ExprKind::MacCall(mac_call);
        //
        //     ast::ptr::P(ast::Expr { kind, ..expr.clone().into_inner() })
        // };

        annotate_expr(self.tcx, self.acx, original_ref, expr);
    }
}
