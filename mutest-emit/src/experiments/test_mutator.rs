use ast::token::DelimToken;
use rustc_ast as ast;
use rustc_ast::token::{Token, TokenKind};
use rustc_ast::tokenstream::{DelimSpan, Spacing, TokenStream, TokenTree};
use rustc_ast::mut_visit::MutVisitor;
use rustc_ast::ptr::P;
use rustc_expand::base::ExtCtxt;
use rustc_span::symbol::Ident;
use rustc_span::Symbol;
use smallvec::{smallvec, SmallVec};

use crate::analysis::tests::Test;

struct TestMutator<'ecx, 'op, 'tst> {
    ecx: &'op mut ExtCtxt<'ecx>,
    tests: &'tst Vec<Test>,
}

impl MutVisitor for TestMutator<'_, '_, '_> {
    fn flat_map_item(&mut self, i: P<ast::Item>) -> SmallVec<[P<ast::Item>; 1]> {
        let mut item = i.into_inner();

        if let ast::ItemKind::Mod(..) = item.kind {
            ast::mut_visit::noop_visit_item_kind(&mut item.kind, self);
        }

        println!("visiting item {} ({})", item.ident, item.id);

        if let Some(test) = self.tests.iter().find(|&test| test.item.id == item.id) {
            println!("encountered test fn");

            if let ast::ItemKind::Fn(box ast::Fn { body: Some(ref mut test_fn_body), .. }) = item.kind {
                // let mut body = test_fn_body.into_inner();

                let msg = format!("[injected by mutest_emit::experiments::test_mutator::TestMutator] hello from {}", test.descriptor.ident);
                // let msg = format!("[injected by mutest_emit::experiments::test_mutator::TestMutator] hello from {}, {{{{ foo_1: {{}} }}}}", test.descriptor.ident);

                let stmt = self.ecx.stmt_expr(self.ecx.expr(item.span, ast::ExprKind::MacCall(ast::MacCall {
                    path: self.ecx.path(item.span, vec![
                        Ident::new(Symbol::intern("println"), item.span),
                    ]),
                    args: P(ast::MacArgs::Delimited(
                        DelimSpan::from_single(item.span),
                        ast::MacDelimiter::Parenthesis,
                        TokenStream::new(vec![
                            (
                                TokenTree::Token(Token {
                                    span: item.span,
                                    kind: TokenKind::Literal(ast::token::Lit {
                                        kind: ast::token::LitKind::Str,
                                        symbol: Symbol::intern(&msg),
                                        suffix: None,
                                    }),
                                }),
                                Spacing::Alone,
                            ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Comma,
                            //     }),
                            //     Spacing::Alone,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Ident(Symbol::intern("crate"), false),
                            //     }),
                            //     Spacing::Alone,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::ModSep,
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Ident(Symbol::intern("mutest_generated"), false),
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::ModSep,
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Ident(Symbol::intern("ACTIVE_MUTANT_HANDLE"), false),
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Dot,
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Ident(Symbol::intern("borrow"), false),
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Delimited(
                            //         DelimSpan::from_single(item.span),
                            //         DelimToken::Paren,
                            //         TokenStream::new(vec![])
                            //     ),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Dot,
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Ident(Symbol::intern("unwrap"), false),
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Delimited(
                            //         DelimSpan::from_single(item.span),
                            //         DelimToken::Paren,
                            //         TokenStream::new(vec![])
                            //     ),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Dot,
                            //     }),
                            //     Spacing::Joint,
                            // ),
                            // (
                            //     TokenTree::Token(Token {
                            //         span: item.span,
                            //         kind: TokenKind::Ident(Symbol::intern("foo_1"), false),
                            //     }),
                            //     Spacing::Joint,
                            // ),
                        ]),
                    )),
                    prior_type_ascription: None,
                })));

                test_fn_body.stmts.insert(0, stmt);
            }

            // return smallvec![P(item)];
        }

        smallvec![P(item)]
    }
}

pub fn prepend_message_to_tests(ecx: &mut ExtCtxt<'_>, tests: &Vec<Test>, krate: &mut ast::Crate) {
    let mut mutator = TestMutator { ecx, tests };
    mutator.visit_crate(krate);
}
