/// Implement structures which allow Carp to communicate via the UCI protocol
/// https://en.wikipedia.org/wiki/Universal_Chess_Interface
use std::{
    io,
    io::BufRead,
    str::FromStr,
    sync::atomic::{AtomicBool, Ordering},
    sync::{mpsc, Arc},
    thread,
};

use crate::engine::{clock::*, position::*, thread::*, tt::*};

const NAME: &str = "Carp";
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

const ENGINE_OPTIONS: &str = "
option name Hash type spin default 16 min 1 max 1048576 
option name Threads type spin default 1 min 1 max 512";

/// UCI reader responsible of reading input and forwarding commands to the main controller
/// We keep a global stop flag that we hand out through an reference counted pointer to all search
/// threads, to be able to stop the search upon receiving the stop/quit command.
///
/// implementation inspired by weiawaga/asymptote
pub struct UCIReader {
    stop: Arc<AtomicBool>,
    controller_tx: mpsc::Sender<UCICommand>,
}

impl Default for UCIReader {
    fn default() -> Self {
        let (tx, rx) = mpsc::channel::<UCICommand>();
        let stop = Arc::new(AtomicBool::new(false));
        let thread_stop = stop.clone();
        thread::spawn(move || UCIController::run(rx, thread_stop));

        UCIReader {
            stop,
            controller_tx: tx,
        }
    }
}

/// Handles communication with the gui and communicates with main engine thread
impl UCIReader {
    /// Start UCI I/O loop
    pub fn run(&self) {
        println!("{NAME} v{VERSION} by {AUTHOR}");

        let stream = io::stdin().lock();

        for line in stream.lines().map(|l| l.expect("Parsing error!")) {
            match line.parse::<UCICommand>() {
                Ok(command) => {
                    match command {
                        UCICommand::Uci => {
                            println!("id name {NAME} {VERSION}");
                            println!("id author {AUTHOR}");
                            println!("{ENGINE_OPTIONS}");
                            println!("uciok");
                        }
                        UCICommand::IsReady => {
                            println!("readyok");
                        }
                        UCICommand::Stop => self.stop.store(true, Ordering::SeqCst), // strict ordering
                        UCICommand::Quit => return,
                        _ => self.controller_tx.send(command).unwrap(),
                    }
                }
                Err(e) => eprintln!("{e}"),
            };
        }
    }
}

/// Enum to represent UCI commands (and extra debug commands)
enum UCICommand {
    // Main UCI commands
    UciNewGame,
    Uci,
    IsReady,
    Option(String, String),
    Position(Box<Position>),
    Go(TimeControl),
    Quit,
    Stop,

    // Extra debug commands
    Perft(usize),
    Print,
    Eval,
}

/// Parse string into uci command
impl FromStr for UCICommand {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();

        match tokens.next() {
            Some("ucinewgame") => Ok(Self::UciNewGame),
            Some("uci") => Ok(Self::Uci),
            Some("isready") => Ok(Self::IsReady),
            Some("setoption") => {
                let opt_name: String = match tokens.next() {
                    Some("name") => tokens.next().ok_or("No option name!")?.to_owned(),
                    _ => return Err("Invalid option command"),
                };
                let opt_value: String = match tokens.next() {
                    Some("value") => tokens.next().ok_or("No option value!")?.to_owned(),
                    _ => return Err("Invalid option command"),
                };

                Ok(Self::Option(opt_name, opt_value))
            }
            Some("perft") => match tokens.next().ok_or("No option value!")?.parse() {
                Ok(d) => Ok(Self::Perft(d)),
                _ => Err("Could not parse depth!"),
            },
            Some("print") => Ok(Self::Print),
            Some("eval") => Ok(Self::Eval),
            Some("position") => Ok(Self::Position(Box::new(
                tokens.collect::<Vec<&str>>().join(" ").parse()?,
            ))),
            Some("go") => Ok(Self::Go(tokens.collect::<Vec<&str>>().join(" ").parse()?)),
            Some("stop") => Ok(Self::Stop),
            Some("quit") => Ok(Self::Quit),
            _ => Err("Error parsing command!"),
        }
    }
}

/// Main runnable controller for the engine, handling search commands.
struct UCIController();

impl UCIController {
    /// Directly handle the "active" uci commands forwarded by the controller.
    /// Meant to be run on a separate thread, to allow for async search interruption.
    fn run(rx: mpsc::Receiver<UCICommand>, stop: Arc<AtomicBool>) {
        let mut position = Position::default();
        let mut tt = TT::default();
        let mut thread_pool = ThreadPool::new(stop);

        for command in &rx {
            match command {
                UCICommand::UciNewGame => {
                    position = Position::default();
                    thread_pool.reset();
                    tt.clear();
                }

                UCICommand::Option(name, value) => match &name[..] {
                    "Hash" => match value.parse::<usize>() {
                        Ok(size) if size > 1 => tt.resize(size),
                        _ => eprintln!("Could not parse hash option value!"),
                    },
                    "Threads" => match value.parse::<usize>() {
                        Ok(size) if size > 1 => thread_pool.resize(size),
                        _ => eprintln!("Could not parse threads option value!"),
                    },
                    _ => eprintln!("Unsupported option command!"),
                },

                UCICommand::Perft(d) => {
                    position.board.perft(d);
                }

                UCICommand::Print => {
                    println!("{}", position.board);
                }

                UCICommand::Eval => {
                    println!("Static evaluation: {}", position.evaluate());
                }

                UCICommand::Position(pos) => {
                    position = *pos;
                }

                UCICommand::Go(tc) => {
                    tt.increment_age();
                    println!(
                        "bestmove {}",
                        thread_pool.deploy_search(&mut position, &tt, tc),
                    );
                }

                _ => eprintln!("Unexpected UCI command!"),
            }
        }
    }
}
