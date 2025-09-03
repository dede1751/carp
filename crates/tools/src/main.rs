/// Tools encapsulates various cli utilities for engine development
/// Will get expanded as more functionality is introduced
mod binpack;
mod datagen;

#[cfg(feature = "train")]
mod train;

use clap::{Parser, Subcommand};

/// ANSI escape codes for coloured output
pub const DEFAULT: &str = "\x1b[0m";
pub const WHITE: &str = "\x1b[38;5;15m";
pub const ORANGE: &str = "\x1b[38;5;208m";
pub const GREEN: &str = "\x1b[38;5;40m";
pub const RED: &str = "\x1b[38;5;196m";

/// CLI tools for Carp development.
/// Each tool is an individual subcommand.
#[derive(Parser)]
#[command(author, version, long_about = None)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Command>,
}

/// Commands represent the various tools available.
/// Each tool has its own Option struct defined in its module.
#[derive(Subcommand)]
enum Command {
    Datagen(datagen::DatagenOptions),
    #[cfg(feature = "train")]
    Train(train::TrainerOptions),
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Some(Command::Datagen(opts)) => datagen::run_datagen(opts),
        #[cfg(feature = "train")]
        Some(Command::Train(opts)) => train::run_trainer(opts),
        _ => eprintln!("{ORANGE}No valid command provided! Exiting. . . "),
    }
}
