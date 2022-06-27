pub use rustc_span::*;
pub use rustc_span::symbol::Ident;

macro symbols($($sym:ident),* $(,)?) {
    use lazy_static::lazy_static;
    use rustc_span::Symbol;

    lazy_static! {
        $(
            pub static ref $sym: Symbol = Symbol::intern(stringify!($sym));
        )*
    }
}

#[allow(non_upper_case_globals)]
pub mod sym {
    pub use rustc_span::sym::*;

    super::symbols! {
        and_then,
        as_ref,
        borrow,
        default,
        non_upper_case_globals,
        println,
        unused_parens,

        ACTIVE_MUTANT_HANDLE,
        ActiveMutantHandle,
        display_location,
        display_name,
        harness,
        id,
        Mutant,
        mutant,
        MutantMeta,
        MUTANTS,
        mutation,
        mutations,
        MutationMeta,
        mutest,
        mutest_generated,
        mutest_main_static,
        mutest_runtime,
        substitutions,
        SubstMap,
        SubstMeta,
        reachable_from,
        tests,
        undetected_diagnostic,
    }
}

pub mod kw {
    pub use rustc_span::symbol::kw::*;
}

macro paths {
    (@IS_PATH_GLOBAL, ::$path:path) => { true },
    (@IS_PATH_GLOBAL, $path:path) => { false },

    (@EXPAND_PATH_SEGMENTS, $sp:ident, $(::)?$($segment:ident)::+) => {
        vec![
            $(Ident::new(Symbol::intern(stringify!($segment)), $sp),)+
        ]
    },

    (@STRINGIFY_PATH, $path:path) => { stringify!($path) },

    ($($ident:ident ($($path:tt)*)),* $(,)?) => {
        use crate::codegen::ast;

        $(
            #[doc = concat!("`", paths!(@STRINGIFY_PATH, $($path)*), "`")]
            pub fn $ident(sp: Span) -> ast::Path {
                ast::mk::path_args(sp,
                    paths!(@IS_PATH_GLOBAL, $($path)*),
                    paths!(@EXPAND_PATH_SEGMENTS, sp, $($path)*),
                    vec![]
                )
            }
        )*
    },
}

#[allow(non_snake_case)]
pub mod path {
    super::paths! {
        Default (::core::default::Default),
        default (::core::default::Default::default),
        None (::core::option::Option::None),
        Option (::core::option::Option),
        Some (::core::option::Option::Some),
        TestDescAndFn (::test::TestDescAndFn),

        ACTIVE_MUTANT_HANDLE (crate::mutest_generated::ACTIVE_MUTANT_HANDLE),
        ActiveMutantHandle (::mutest_runtime::ActiveMutantHandle),
        active_mutant_handle_init_empty (::mutest_runtime::ActiveMutantHandle::empty),
        harness (::mutest_generated::harness),
        MutantMeta (::mutest_runtime::MutantMeta),
        MUTANTS (crate::mutest_generated::MUTANTS),
        mutations (crate::mutest_generated::mutations),
        MutationMeta (::mutest_runtime::MutationMeta),
        mutest_main_static (::mutest_runtime::mutest_main_static),
        static_map (::mutest_runtime::static_map),
        SubstMap (crate::mutest_generated::SubstMap),
        SubstMeta (::mutest_runtime::SubstMeta),
        wrap (::mutest_runtime::wrap),
    }
}
