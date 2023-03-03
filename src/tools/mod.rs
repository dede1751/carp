/// Module to encapsulate various cli utilities for engine development
/// Will get expanded as more functionality is introduced

mod bench;


pub fn parse_cli() {
    if std::env::args().nth(1).as_deref() == Some("bench") {
        bench::run_benchmark();
        std::process::exit(0)
    }
}