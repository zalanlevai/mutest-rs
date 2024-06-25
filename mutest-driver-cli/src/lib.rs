#![feature(decl_macro)]

use std::iter;

macro opts(
    $all:ident where
    $($ident:ident = $name:expr);*;
) {
    $(pub const $ident: &str = $name;)*
    pub const $all: &[&str] = &[$($ident,)*];
}

pub mod mutation_operators {
    crate::opts! { ALL where
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
        .subcommand(clap::Command::new("print-targets")
            .display_order(5)
            .about("Print list of functions targeted for mutation at the specified depth.")
        )
        .subcommand(clap::Command::new("print-conflict-graph")
            .display_order(4)
            .about("Print mutation conflict graph.")
            .arg(clap::arg!(--compatibility "Print mutation compatibility graph (a.k.a. the complement graph) instead of conflict graph."))
            .arg(clap::arg!(--"exclude-unsafe" "Exclude unsafe mutations from the graph, only listing safe mutations."))
            .arg(clap::arg!(--format [FORMAT] "Format to print the graph in.").value_parser(["simple", "graphviz"]).default_value("simple"))
        )
        .subcommand(clap::Command::new("print-mutants")
            .display_order(3)
            .about("Print list of generated mutations, grouped into mutant batches.")
        )
        .subcommand(clap::Command::new("print-code")
            .display_order(2)
            .about("Print the generated code of the test harness.")
        )
        .subcommand(clap::Command::new("build")
            .display_order(1)
            .about("Build the test harness.")
        )
        // Arguments
        .arg(clap::arg!(--safe "Avoid mutating code in contexts which contain `unsafe` blocks. [default]").display_order(101))
        .arg(clap::arg!(--cautious "Produce unsafe mutations in contexts which contain `unsafe` blocks.").display_order(102))
        .arg(clap::arg!(--risky "Produce safe mutations in contexts which contain `unsafe` blocks.").display_order(103))
        .arg(clap::arg!(--unsafe "Mutate code in `unsafe` blocks.").display_order(104))
        .group(clap::ArgGroup::new("unsafe-targeting").args(&["safe", "cautious", "risky", "unsafe"]).multiple(false))
        .arg(clap::arg!(--"mutation-operators" [MUTATION_OPERATORS] "Mutation operators to apply to the code, separated by commas.").value_delimiter(',').value_parser(iter::once(&"all").chain(mutation_operators::ALL).collect::<Vec<_>>()).default_value("all").display_order(105))
        .arg(clap::arg!(--"call-graph-depth" [CALL_GRAPH_DEPTH] "Depth of call graph analysis.").default_value("1000").value_parser(clap::value_parser!(usize)).display_order(110))
        .arg(clap::arg!(-d --depth [DEPTH] "Callees of each test function are mutated up to the specified depth.").default_value("3").value_parser(clap::value_parser!(usize)).display_order(110))
        .arg(clap::arg!(--"mutant-batch-algorithm" [MUTANT_BATCH_ALGORITHM] "Algorithm to use to batch mutations into mutants.").value_parser(["greedy", #[cfg(feature = "random")] "random", #[cfg(feature = "random")] "simulated-annealing", "none"]).default_value("none").display_order(199))
        .arg(clap::arg!(--"mutant-batch-size" [MUTANT_BATCH_SIZE] "Maximum number of mutations to batch into a single mutant.").default_value("1").value_parser(clap::value_parser!(usize)).display_order(199))
        .arg(clap::arg!(--"mutant-batch-greedy-ordering-heuristic" [MUTANT_BATCH_GREEDY_ORDERING_HEURISTIC] "Ordering heuristic to use for `greedy` mutation batching algorithm.").value_parser(["conflicts", "reverse-conflicts", #[cfg(feature = "random")] "random", "none"]).default_value("reverse-conflicts").display_order(199))
        .arg(clap::arg!(--timings "Print timing information for each completed pass.").display_order(100))
        .arg(clap::arg!(-v --verbose "Print more verbose information during execution.").action(clap::ArgAction::Count).default_value("0").display_order(100))
        // Experimental Flags
        .arg(clap::arg!(--"Zsanitize-macro-expns" "Sanitize the identifiers and paths in the expanded output of bang-style macro invocations.").display_order(500))
        // Information
        // FIXME: Regression; the `help` subcommand can no longer be customized, so the about text does not match that
        //        of the help flags.
        .arg(clap::arg!(-h --help "Print help information; this message or the help of the given subcommand.").action(clap::ArgAction::Help).global(true))
        .arg(clap::arg!(-V --version "Print version information.").action(clap::ArgAction::Version).global(true));

    #[cfg(feature = "random")]
    let cmd = cmd
        .arg(clap::arg!(--"mutant-batch-seed" [MUTANT_BATCH_SEED] "Random seed to use for randomness during mutation batching.").display_order(199))
        .arg(clap::arg!(--"mutant-batch-greedy-epsilon" [MUTANT_BATCH_GREEDY_EPSILON] "Optional epsilon parameter for `greedy` mutation batching algorithm, used to control the probability of random mutation assignment.").default_value("0").value_parser(clap::value_parser!(f64)).display_order(199));

    cmd
}
