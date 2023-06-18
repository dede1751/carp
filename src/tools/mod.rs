/// Module to encapsulate various cli utilities for engine development
/// Will get expanded as more functionality is introduced
mod datagen;
mod merge;

use clap::Parser;

/// ANSI escape codes for coloured output
pub const DEFAULT: &str = "\x1b[0m";
pub const WHITE: &str = "\x1b[38;5;15m";
pub const ORANGE: &str = "\x1b[38;5;208m";
pub const GREEN: &str = "\x1b[38;5;40m";
pub const RED: &str = "\x1b[38;5;196m";

/// Carp, a didactic chess engine.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Run nnue datagen, formatted like so: '{games}g-{threads}t-{value}[d/n]'
    #[arg(short, long, value_name = "OPTIONS")]
    datagen: Option<String>,

    /// Merge and dedup all datagen files in the given directory
    #[clap(short, long, value_parser, value_name = "PATH")]
    pub merge: Option<std::path::PathBuf>,
}

pub fn parse_cli() {
    let args = Cli::parse();

    if let Some(option_string) = args.datagen {
        match option_string.parse() {
            Ok(options) => {
                datagen::run_datagen(options);
                std::process::exit(0)
            }
            Err(err) => {
                eprintln!("{ORANGE}{err}");
                std::process::exit(1)
            }
        }
    }

    if let Some(marge_dir) = args.merge {
        match merge::merge(marge_dir) {
            Ok(_) => std::process::exit(0),
            Err(err) => {
                eprintln!("{ORANGE}{err}");
                std::process::exit(1)
            }
        }
    }
}
