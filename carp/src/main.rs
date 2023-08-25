/// Carp UCI Chess Engine by Andrea Sgobbi
///
/// This is a didactic chess engine for both approaching chess programming and learning to code
/// in Rust.

fn main() {
    chess::init_all_tables();

    if std::env::args().nth(1).as_deref() == Some("bench") {
        carp::bench::run_benchmark();
        return;
    }

    carp::uci::UCIReader::default().run();
}
