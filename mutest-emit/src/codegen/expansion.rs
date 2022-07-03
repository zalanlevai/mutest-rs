use rustc_expand::base::ResolverExpand;
use rustc_resolve::Resolver;

use crate::codegen::ast;
use crate::codegen::symbols::{DUMMY_SP, sym};
use crate::codegen::symbols::hygiene::AstPass;

pub const GENERATED_CODE_PRELUDE: &str = r#"
#![allow(unused_features)]
#![allow(unused_imports)]

#![feature(rustc_attrs)]
#![feature(int_error_internals)]
#![feature(fmt_internals)]
#![feature(str_internals)]
#![feature(sort_internals)]
#![feature(print_internals)]
#![feature(allocator_internals)]
#![feature(char_error_internals)]
#![feature(libstd_sys_internals)]
#![feature(thread_local_internals)]
#![feature(libstd_thread_internals)]

#![feature(box_syntax)]
#![feature(core_intrinsics)]
#![feature(core_panic)]
#![feature(derive_clone_copy)]
#![feature(derive_eq)]
#![feature(no_coverage)]
#![feature(rustc_private)]
#![feature(structural_match)]
"#;

pub fn insert_generated_code_crate_refs(resolver: &mut Resolver, krate: &mut ast::Crate) {
    let expn_id = resolver.expansion_for_ast_pass(
        DUMMY_SP,
        AstPass::StdImports,
        &[sym::rustc_attrs],
        None,
    );
    let def_site = DUMMY_SP.with_def_site_ctxt(expn_id.to_expn_id());

    // extern crate alloc;
    if !krate.items.iter().any(|item| ast::inspect::is_extern_crate_decl(item, sym::alloc)) {
        krate.items.push(ast::mk::item_extern_crate(def_site, sym::alloc, None));
    }
}
