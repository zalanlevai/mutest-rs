#![feature(decl_macro)]

use std::path::PathBuf;

pub macro opts(
    $all:ident, $possible_values_vis:vis $possible_values:ident where
    $($(#[$attr:meta])* $ident:ident = $name:expr; $([$help:expr])?)*
) {
    $($(#[$attr])* pub const $ident: &str = $name;)*
    pub const $all: &[&str] = &[$($(#[$attr])* $ident,)*];

    $possible_values_vis fn $possible_values() -> Vec<clap::builder::PossibleValue> {
        vec![
            clap::builder::PossibleValue::new("all"),
            $($(#[$attr])* clap::builder::PossibleValue::new($name)$(.help($help))?,)*
        ]
    }
}

pub macro exclusive_opts(
    $possible_values_vis:vis $possible_values:ident where
    $($(#[$attr:meta])* $ident:ident = $name:expr; $([$help:expr])?)*
) {
    $($(#[$attr])* pub const $ident: &str = $name;)*

    $possible_values_vis fn $possible_values() -> Vec<clap::builder::PossibleValue> {
        vec![$($(#[$attr])* clap::builder::PossibleValue::new($name)$(.help($help))?,)*]
    }
}

pub mod mutation_operators {
    crate::opts! { ALL, pub(crate) possible_values where
        ARG_DEFAULT_SHADOW = "arg_default_shadow";
        BIT_OP_OR_AND_SWAP = "bit_op_or_and_swap";
        BIT_OP_OR_XOR_SWAP = "bit_op_or_xor_swap";
        BIT_OP_SHIFT_DIR_SWAP = "bit_op_shift_dir_swap";
        BIT_OP_XOR_AND_SWAP = "bit_op_xor_and_swap";
        BOOL_EXPR_NEGATE = "bool_expr_negate";
        CALL_DELETE = "call_delete";
        CALL_VALUE_DEFAULT_SHADOW = "call_value_default_shadow";
        CONTINUE_BREAK_SWAP = "continue_break_swap";
        EQ_OP_INVERT = "eq_op_invert";
        LOGICAL_OP_AND_OR_SWAP = "logical_op_and_or_swap";
        MATH_OP_ADD_MUL_SWAP = "math_op_add_mul_swap";
        MATH_OP_ADD_SUB_SWAP = "math_op_add_sub_swap";
        MATH_OP_DIV_REM_SWAP = "math_op_div_rem_swap";
        MATH_OP_MUL_DIV_SWAP = "math_op_mul_div_swap";
        RANGE_LIMIT_SWAP = "range_limit_swap";
        RELATIONAL_OP_EQ_SWAP = "relational_op_eq_swap";
        RELATIONAL_OP_INVERT = "relational_op_invert";
    }
}

pub mod mutant_batch_algorithm {
    crate::exclusive_opts! { pub(crate) possible_values where
        GREEDY = "greedy";
        RANDOM = "random";
        SIMULATED_ANNEALING = "simulated-annealing";
        NONE = "none";
    }
}

pub mod mutant_batch_greedy_ordering_heuristic {
    crate::exclusive_opts! { pub(crate) possible_values where
        CONFLICTS = "conflicts";
        REVERSE_CONFLICTS = "reverse-conflicts";
        RANDOM = "random";
        NONE = "none";
    }
}

pub mod print {
    crate::opts! { ALL, pub(crate) possible_values where
        TESTS = "tests"; ["Print list of test cases."]
        TARGETS = "targets"; ["Print list of functions targeted for mutation at the specified depth."]
        CALL_GRAPH = "call-graph"; ["Print call graph of test cases."]
        CONFLICT_GRAPH = "conflict-graph"; ["Print mutation conflict graph."]
        COMPATIBILITY_GRAPH = "compatibility-graph"; ["Print mutation compatibility graph (i.e. the complement graph of the conflict graph)."]
        MUTATIONS = "mutations"; ["Print list of generated mutations, optionally grouped into mutation batches."]
        CODE = "code"; ["Print the generated code of the test harness."]
    }
}

pub mod graph_format {
    crate::exclusive_opts! { pub(crate) possible_values where
        SIMPLE = "simple";
        GRAPHVIZ = "graphviz";
    }
}

pub mod call_graph_non_local_call_view {
    crate::exclusive_opts! { pub(crate) possible_values where
        COLLAPSE = "collapse"; ["Non-local items are hidden, only calls to local items through non-local items are shown."]
        EXPAND = "expand"; ["All calls to non-local items are shown."]
    }
}

pub mod verify {
    crate::opts! { ALL, pub(crate) possible_values where
        AST_LOWERING = "ast_lowering";
    }
}

pub fn command() -> clap::Command {
    let cmd = clap::command!("cargo mutest")
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        .disable_help_flag(true)
        .disable_version_flag(true)
        .styles({
            use clap::builder::styling::*;
            Styles::styled()
                .header(Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightGreen))).bold())
                .usage(Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightGreen))).bold())
                .literal(Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightBlue))).bold())
                .placeholder(Style::new().fg_color(Some(Color::Ansi(AnsiColor::BrightBlue))))
        })
        // Subcommands
        .subcommand(clap::Command::new("print")
            .display_order(2)
            .about("Print information about analysis, without building.")
        )
        .subcommand(clap::Command::new("build")
            .display_order(1)
            .about("Build the test harness.")
        )
        // Mutation-related Arguments
        .arg(clap::arg!(--safe "Avoid mutating code in contexts which contain `unsafe` blocks. [default]").display_order(111))
        .arg(clap::arg!(--cautious "Produce unsafe mutations in contexts which contain `unsafe` blocks.").display_order(112))
        .arg(clap::arg!(--risky "Produce safe mutations in contexts which contain `unsafe` blocks.").display_order(113))
        .arg(clap::arg!(--unsafe "Mutate code in `unsafe` blocks.").display_order(114))
        .group(clap::ArgGroup::new("unsafe-targeting").args(&["safe", "cautious", "risky", "unsafe"]).multiple(false))
        .arg(clap::arg!(--"mutation-operators" [MUTATION_OPERATORS] "Mutation operators to apply to the code, separated by commas.").value_delimiter(',').value_parser(mutation_operators::possible_values()).default_value("all").display_order(115))
        .arg(clap::arg!(--"call-graph-depth-limit" [CALL_GRAPH_DEPTH_LIMIT] "Limit depth of call graph analysis, which is complete by default.").value_parser(clap::value_parser!(usize)).display_order(150))
        .arg(clap::arg!(--"call-graph-trace-length-limit" [CALL_GRAPH_TRACE_LENGTH_LIMIT] "Limit maximum length of analyzed call traces during call graph analysis, which is complete by default.").value_parser(clap::value_parser!(usize)).display_order(150))
        .arg(clap::arg!(-d --depth [DEPTH] "Callees of each test function are mutated up to the specified depth.").default_value("3").value_parser(clap::value_parser!(usize)).display_order(150))
        .arg(clap::arg!(--"mutant-batch-algorithm" [MUTANT_BATCH_ALGORITHM] "Algorithm to use to optionally batch mutations into parallel groups.").value_parser(mutant_batch_algorithm::possible_values()).default_value(mutant_batch_algorithm::NONE).display_order(199))
        .arg(clap::arg!(--"mutant-batch-size" [MUTANT_BATCH_SIZE] "Maximum number of mutations to batch into a single batch.").default_value("1").value_parser(clap::value_parser!(usize)).display_order(199))
        .arg(clap::arg!(--"mutant-batch-seed" [MUTANT_BATCH_SEED] "Random seed to use for randomness during mutation batching.").display_order(199))
        .arg(clap::arg!(--"mutant-batch-greedy-ordering-heuristic" [MUTANT_BATCH_GREEDY_ORDERING_HEURISTIC] "Ordering heuristic to use for `greedy` mutation batching algorithm.").value_parser(mutant_batch_greedy_ordering_heuristic::possible_values()).default_value(mutant_batch_greedy_ordering_heuristic::REVERSE_CONFLICTS).display_order(199))
        .arg(clap::arg!(--"mutant-batch-greedy-epsilon" [MUTANT_BATCH_GREEDY_EPSILON] "Optional epsilon parameter for `greedy` mutation batching algorithm, used to control the probability of random mutation assignment.").default_value("0").value_parser(clap::value_parser!(f64)).display_order(199))
        // Printing-related Arguments
        .arg(clap::arg!(--timings "Print timing information for each completed pass.").display_order(100))
        .arg(clap::arg!(-v --verbose "Print more verbose information during execution.").action(clap::ArgAction::Count).default_value("0").display_order(100))
        .arg(clap::arg!(--print [PRINT] "Print additional information during analysis. Multiple may be specified, separated by commas.").value_delimiter(',').value_parser(print::possible_values()).display_order(101))
        .arg(clap::arg!(--"graph-exclude-unsafe" "Exclude unsafe mutations from the graph, only listing safe mutations.").display_order(102))
        .arg(clap::arg!(--"graph-format" [GRAPH_FORMAT] "Format to print the graph in.").value_parser(graph_format::possible_values()).default_value(graph_format::SIMPLE).display_order(102))
        .arg(clap::arg!(--"call-graph-non-local-calls" [CALL_GRAPH_NON_LOCAL_CALL_VIEW] "Mode to display non-local calls in the call graph.").value_parser(call_graph_non_local_call_view::possible_values()).default_value(call_graph_non_local_call_view::COLLAPSE).display_order(103))
        .arg(clap::arg!(--"call-graph-filter-tests" [CALL_GRAPH_FILTER_TESTS] "Filter tests to display the call graph for. Multiple may be specified, separated by commas.").value_delimiter(',').display_order(103))
        // Experimental Flags
        .arg(clap::arg!(--"Zwrite-json" [OUT_DIR] "Write JSON metadata files into the specified directory.").value_parser(clap::value_parser!(PathBuf)).display_order(500))
        .arg(clap::arg!(--Zverify [VERIFY] "Perform additional checks to verify correctness and completeness. Multiple may be specified, separated by commas.").value_delimiter(',').value_parser(verify::possible_values()).display_order(500))
        .arg(clap::arg!(--"Zno-sanitize-macro-expns" "Skip sanitizing the identifiers and paths in the expanded output of macro invocations. This was the previous behavior and is not recommended.").display_order(500))
        // Information
        // FIXME: Regression; the `help` subcommand can no longer be customized, so the about text does not match that
        //        of the help flags.
        .arg(clap::arg!(-h --help "Print help information; this message or the help of the given subcommand.").action(clap::ArgAction::Help).global(true))
        .arg(clap::arg!(-V --version "Print version information.").action(clap::ArgAction::Version).global(true));

    cmd
}
