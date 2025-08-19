pub use rustc_span::*;
pub use rustc_span::symbol::Ident;

use std::cmp::{Ord, Ordering};
use std::mem;

pub fn span_diagnostic_ord(a: Span, b: Span) -> Ordering {
    let a_data = a.data();
    let b_data = b.data();

    // HACK: There is currently no way of publicly accessing
    //       the `SyntaxContext`'s underlying `u32`
    //       without invoking the `Debug` formatter.
    // SAFETY: `SyntaxContext` is a single `u32`.
    let a_ctxt_idx = unsafe { mem::transmute::<_, u32>(a_data.ctxt) };
    // SAFETY: `SyntaxContext` is a single `u32`.
    let b_ctxt_idx = unsafe { mem::transmute::<_, u32>(b_data.ctxt) };

    Ord::cmp(&a_data.lo, &b_data.lo)
        .then(Ord::cmp(&a_data.hi, &b_data.hi))
        .then(Ord::cmp(&a_ctxt_idx, &b_ctxt_idx))
}

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
        active_mutant_handle,
        batch_id,
        display_location,
        display_name,
        harness,
        id,
        META_MUTANT,
        Mutant,
        mutant,
        MUTANTS,
        mutation,
        mutations,
        mutation_parallelism,
        MutationMeta,
        mutest,
        mutest_generated,
        mutest_main_static,
        mutest_runtime,
        op_name,
        reachable_from,
        substitutions,
        SubstMap,
        SubstMeta,
        subst_at,
        subst_at_unchecked,
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
        panic (::core::panic),
        Some (::core::option::Option::Some),
        TestDescAndFn (::test::TestDescAndFn),

        ACTIVE_MUTANT_HANDLE (crate::mutest_generated::ACTIVE_MUTANT_HANDLE),
        ActiveMutantHandle (::mutest_runtime::ActiveMutantHandle),
        active_mutant_handle_init_empty (::mutest_runtime::ActiveMutantHandle::empty),
        BatchedMutantMeta (::mutest_runtime::BatchedMutantMeta),
        harness (crate::mutest_generated::harness),
        is_test_thread_active (::mutest_runtime::is_test_thread_active),
        META_MUTANT (crate::mutest_generated::META_MUTANT),
        MetaMutant (::mutest_runtime::MetaMutant),
        MUTANTS (crate::mutest_generated::MUTANTS),
        mutations (crate::mutest_generated::mutations),
        MutationMeta (::mutest_runtime::MutationMeta),
        MutationParallelismNone (::mutest_runtime::MutationParallelism::None),
        MutationParallelismBatched (::mutest_runtime::MutationParallelism::Batched),
        MutationSafetySafe (::mutest_runtime::MutationSafety::Safe),
        MutationSafetyTainted (::mutest_runtime::MutationSafety::Tainted),
        MutationSafetyUnsafe (::mutest_runtime::MutationSafety::Unsafe),
        mutest_main_static (::mutest_runtime::mutest_main_static),
        StandaloneMutantMeta (::mutest_runtime::StandaloneMutantMeta),
        static_map (::mutest_runtime::static_map),
        SubstMap (crate::mutest_generated::SubstMap),
        SubstMapTrait (::mutest_runtime::SubstMap),
        subst_map_array (::mutest_runtime::subst_map_array),
        SubstMeta (::mutest_runtime::SubstMeta),
    }
}
