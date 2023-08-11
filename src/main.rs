/// Carp UCI Chess Engine by Andrea Sgobbi
///
/// This is a didactic chess engine for both approaching chess programming and learning to code
/// in Rust.
pub mod chess;
pub mod engine;

#[cfg(feature = "tools")]
pub mod tools;

fn main() {
    chess::init_all_tables();

    if std::env::args().nth(1).as_deref() == Some("bench") {
        engine::bench::run_benchmark();
        return;
    }

    #[cfg(feature = "tools")]
    tools::parse_cli();

    engine::UCIReader::default().run();
}
