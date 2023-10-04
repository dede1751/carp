/// Carp UCI Chess Engine by Andrea Sgobbi
///
/// This is a didactic chess engine for both approaching chess programming and learning to code
/// in Rust.

fn main() {
    if std::env::args().nth(1).as_deref() == Some("bench") {
        match std::env::args().nth(2).as_deref() {
            Some(arg) => engine::bench::run_benchmark(arg.parse().unwrap()),
            None => engine::bench::run_benchmark(13),
        }

        return;
    }

    engine::uci::UCIReader::default().run();
}
