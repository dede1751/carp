/// Carp chess engine by Andrea Sgobbi
///
/// This is a didactic chess engine for both approaching chess programming and learning to code
/// in Rust.
pub mod chess;
pub mod engine;
pub mod tools;

fn main() {
    chess::init_all_tables();
    tools::parse_cli();

    engine::UCIController::default().run();
}
