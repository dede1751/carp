/// Module to encapsulate various cli utilities for engine development
/// Will get expanded as more functionality is introduced
mod bench;
mod datagen;

use clap::Parser;

/// Carp, a didactic chess engine.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Datagen string: '{games}g-{threads}t-{value}[d/n]}'
    #[arg(short, long)]
    datagen: Option<String>,

    /// Run node benchmark for OpenBench
    #[clap(subcommand)]
    pub bench: Option<Bench>,
}

#[derive(Parser, Debug)]
enum Bench {
    Bench,
}

pub fn parse_cli() {
    let args = Args::parse();

    if args.bench.is_some() {
        bench::run_benchmark();
        std::process::exit(0);
    }

    if let Some(option_string) = args.datagen {
        match option_string.parse() {
            Ok(options) => {
                datagen::run_datagen(options);
                std::process::exit(0)
            }
            Err(err) => {
                eprintln!("{err}");
                std::process::exit(1)
            }
        }
    }
}
