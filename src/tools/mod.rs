/// Module to encapsulate various cli utilities for engine development
/// Will get expanded as more functionality is introduced
mod datagen;
mod merge;

use clap::{Parser, Subcommand};

/// ANSI escape codes for coloured output
pub const DEFAULT: &str = "\x1b[0m";
pub const WHITE: &str = "\x1b[38;5;15m";
pub const ORANGE: &str = "\x1b[38;5;208m";
pub const GREEN: &str = "\x1b[38;5;40m";
pub const RED: &str = "\x1b[38;5;196m";

/// CLI tools for Carp development.
#[derive(Parser)]
#[command(author, version, long_about = None)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    Datagen(datagen::DatagenOptions),
    Merge(merge::MergeOptions),
}

/// Parse command line arguments. Any subcommand will terminate the program after execution.
pub fn parse_cli() {
    let args = Cli::parse();

    if let Some(cmd) = &args.command {
        match cmd {
            Command::Datagen(opts) => datagen::run_datagen(opts),
            Command::Merge(opts) => {
                if let Err(err) = merge::merge(&opts.path) {
                    eprintln!("{ORANGE}{err}");
                    std::process::exit(1)
                }
            },
        }
        std::process::exit(0);
    }
}
