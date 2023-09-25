pub fn command() -> clap::Command<'static> {
    clap::command!("cargo mutest")
        .propagate_version(true)
        .subcommand_required(true)
        .arg_required_else_help(true)
        // Subcommands
        .subcommand(clap::Command::new("print-targets")
            .display_order(4)
            .about("Print list of functions targeted for mutation at the specified depth.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
        )
        .subcommand(clap::Command::new("print-mutants")
            .display_order(3)
            .about("Print list of generated mutations, grouped into mutant batches.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
        )
        .subcommand(clap::Command::new("print-code")
            .display_order(2)
            .about("Print the generated code of the test harness.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
        )
        .subcommand(clap::Command::new("build")
            .display_order(1)
            .about("Build the test harness.")
            // Information
            .arg(clap::arg!(-h --help "Print help information."))
            .arg(clap::arg!(-V --version "Print version information."))
        )
        // Arguments
        .arg(clap::arg!(--safe "Avoid mutating code in contexts which contain `unsafe` blocks.").display_order(100))
        .arg(clap::arg!(--cautious "Produce unsafe mutations in contexts which contain `unsafe` blocks.").display_order(100))
        .arg(clap::arg!(--unsafe "Mutate code in `unsafe` blocks.").display_order(100))
        .group(clap::ArgGroup::new("unsafe-targeting").args(&["safe", "cautious", "unsafe"]).multiple(false))
        .arg(clap::arg!(-d --depth [DEPTH] "Callees of each test function are mutated up to the specified depth.").default_value("3").validator(str::parse::<usize>).display_order(100))
        .arg(clap::arg!(--"mutant-batch-size" [MUTANT_BATCH_SIZE] "Maximum number of mutations to batch into a single mutant.").default_value("1").validator(str::parse::<usize>).display_order(100))
        .arg(clap::arg!(--timings "Print timing information for each completed pass.").display_order(100))
        // Information
        .arg(clap::arg!(-h --help "Print help information; this message or the help of the given subcommand."))
        .arg(clap::arg!(-V --version "Print version information."))
        .subcommand(clap::Command::new("help").about("Print help information; this message or the help of the given subcommand."))
}
