use std::{
    io,
    io::BufRead,
    str::FromStr,
    sync::atomic::{AtomicBool, Ordering},
    sync::{mpsc, Arc},
    thread,
};

use crate::engine::{clock::*, position::*, tt::*};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

const ENGINE_OPTIONS: &str = "
option name Hash type spin default 16 min 1 max 65536 
option name Threads type spin default 1 min 1 max 512";

/// UCI controller responsible of reading input and command the main engine thread
/// uci implementation inspired by weiawaga/asymptote
pub struct UCIController {
    stop: Arc<AtomicBool>,
    thread_tx: mpsc::Sender<UCICommand>,
}

impl Default for UCIController {
    fn default() -> Self {
        let (tx, rx) = mpsc::channel::<UCICommand>();
        let stop = Arc::new(AtomicBool::new(false));
        let thread_stop = stop.clone();
        thread::spawn(move || UCIEngine::new(rx, thread_stop).run());

        UCIController {
            stop,
            thread_tx: tx,
        }
    }
}

/// Handles communication with the gui and communicates with main engine thread
impl UCIController {
    /// Start UCI I/O loop
    pub fn run(&self) {
        println!("{NAME} by {AUTHOR}");

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
                        _ => self.thread_tx.send(command).unwrap(),
                    }
                }
                Err(e) => eprintln!("{e}"),
            };
        }
    }
}

/// Enum to represent UCI commands (and extra debug commands)
enum UCICommand {
    UciNewGame,
    Uci,
    IsReady,
    Option(String, String),
    Perft(usize),
    Position(Box<Position>),
    Go(TimeControl),
    Quit,
    Stop,
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

struct UCIEngine {
    position: Position,
    tt: TT,
    controller_rx: mpsc::Receiver<UCICommand>,
    stop: Arc<AtomicBool>,
    worker_count: usize,
}

impl UCIEngine {
    fn new(rx: mpsc::Receiver<UCICommand>, stop: Arc<AtomicBool>) -> UCIEngine {
        UCIEngine {
            position: Position::default(),
            tt: TT::default(),
            controller_rx: rx,
            stop,
            worker_count: 0,
        }
    }

    /// Dispatch main engine thread.
    /// Handles the "active" uci commands forwarded by the controller and dispatches helpers.
    fn run(&mut self) {
        for command in &self.controller_rx {
            match command {
                UCICommand::UciNewGame => {
                    self.position = Position::default();
                    self.tt.clear();
                }

                UCICommand::Option(name, value) => match &name[..] {
                    "Hash" => match value.parse::<usize>() {
                        Ok(size) => self.tt.resize(size),
                        Err(_) => eprintln!("Could not parse hash option value!"),
                    },
                    "Threads" => match value.parse::<usize>() {
                        Ok(size) => self.worker_count = size,
                        Err(_) => eprintln!("Could not parse threads option value!"),
                    },
                    _ => eprintln!("Unsupported option command!"),
                },

                UCICommand::Perft(d) => {
                    self.position.board.perft(d);
                }

                UCICommand::Position(position) => {
                    self.position = *position;
                }

                UCICommand::Go(tc) => {
                    self.stop.store(false, Ordering::Relaxed);
                    self.tt.increment_age();
                    let main_clock =
                        Clock::new(tc, self.stop.clone(), self.position.white_to_move());
                    let best_move =
                        self.position
                            .smp_search(self.worker_count, main_clock, &self.tt);

                    println!("bestmove {best_move}");
                }

                _ => eprintln!("Unexpected UCI command!"),
            }
        }
    }
}
