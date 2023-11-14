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
        .arg(clap::arg!(--safe "Avoid mutating code in contexts which contain `unsafe` blocks.").display_order(100))
        .arg(clap::arg!(--cautious "Produce unsafe mutations in contexts which contain `unsafe` blocks.").display_order(100))
        .arg(clap::arg!(--unsafe "Mutate code in `unsafe` blocks.").display_order(100))
        .group(clap::ArgGroup::new("unsafe-targeting").args(&["safe", "cautious", "unsafe"]).multiple(false))
        .arg(clap::arg!(-d --depth [DEPTH] "Callees of each test function are mutated up to the specified depth.").default_value("3").value_parser(clap::value_parser!(usize)).display_order(100))
        .arg(clap::arg!(--"mutant-batch-algorithm" [MUTANT_BATCH_ALGORITHM] "Algorithm to use to batch mutations into mutants.").value_parser(["greedy", #[cfg(feature = "random")] "random"]).default_value("greedy").display_order(100))
        .arg(clap::arg!(--"mutant-batch-size" [MUTANT_BATCH_SIZE] "Maximum number of mutations to batch into a single mutant.").default_value("1").value_parser(clap::value_parser!(usize)).display_order(100))
        .arg(clap::arg!(--timings "Print timing information for each completed pass.").display_order(100))
        .arg(clap::arg!(-v --verbose "Print more verbose information during execution.").action(clap::ArgAction::Count).default_value("0").display_order(100))
        // Information
        // FIXME: Regression; the `help` subcommand can no longer be customized, so the about text does not match that
        //        of the help flags.
        .arg(clap::arg!(-h --help "Print help information; this message or the help of the given subcommand.").action(clap::ArgAction::Help).global(true))
        .arg(clap::arg!(-V --version "Print version information.").action(clap::ArgAction::Version).global(true));

    #[cfg(feature = "random")]
    let cmd = cmd
        .arg(clap::arg!(--"mutant-batch-seed" [MUTANT_BATCH_SEED] "Random seed to use for `random` mutation batching algorithm.").display_order(100))
        .arg(clap::arg!(--"mutant-batch-random-attempts" [MUTANT_BATCH_RANDOM_ATTEMPTS] "Number of attempts made to place each mutation into a random mutant by the `random` mutation batching algorithm.").default_value("1").value_parser(clap::value_parser!(usize)).display_order(100));

    cmd
}
