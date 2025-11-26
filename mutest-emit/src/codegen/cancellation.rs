use thin_vec::thin_vec;

use crate::codegen::ast;
use crate::codegen::symbols::{Span, Symbol, path, sym};

pub fn mk_is_test_thread_active_expr(sp: Span) -> Box<ast::Expr> {
    // { extern crate mutest_runtime; mutest_runtime::is_test_thread_active() }
    ast::mk::expr_block(ast::mk::block(sp, thin_vec![
        ast::mk::stmt_item(sp, ast::mk::item_extern_crate(sp, sym::mutest_runtime, None)),
        ast::mk::stmt_expr(ast::mk::expr_call_path(sp, ast::mk::path_local(path::is_test_thread_active(sp)), thin_vec![])),
    ]))
}

pub fn mk_test_thread_cancel_expr(sp: Span) -> Box<ast::Expr> {
    // panic!($panic_message)
    let panic_message = "test thread no longer active: exiting after timeout";
    ast::mk::expr(sp, ast::ExprKind::MacCall(Box::new(ast::MacCall {
        path: path::panic(sp),
        args: Box::new(ast::DelimArgs {
            dspan: ast::tokenstream::DelimSpan::from_single(sp),
            delim: ast::token::Delimiter::Brace,
            tokens: ast::mk::token_stream(vec![
                ast::mk::tt_token_alone(sp, ast::TokenKind::lit(ast::token::LitKind::Str, Symbol::intern(panic_message), None)),
            ]),
        })
    })))
}
