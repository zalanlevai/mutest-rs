use rustc_expand::base::{ExtCtxt, ResolverExpand, LintStoreExpand};
use rustc_expand::expand::ExpansionConfig;
use rustc_session::Session;

type LintStoreExpandDyn<'a> = Option<&'a (dyn LintStoreExpand + 'a)>;

pub fn init_ecx<'a>(sess: &'a Session, crate_name: String, resolver: &'a mut dyn ResolverExpand, lint_store: LintStoreExpandDyn<'a>) -> ExtCtxt<'a> {
    let config = ExpansionConfig::default(crate_name);

    ExtCtxt::new(sess, config, resolver, lint_store)
}

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

#![feature(core_panic)]
"#;
