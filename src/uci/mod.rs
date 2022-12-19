/// UCI integration
mod engine;

use engine::UCIEngine;

use std::{ 
    io,
    io::BufRead, 
    thread,
    sync::{ Arc, mpsc },
    sync::atomic::{ AtomicBool, Ordering },
};

use crate::{
    board_repr::Board,
    clock::TimeControl,
};

const ENGINE_ID: &str = 
"id name Carp 0.4
id author Andrea Sgobbi";

const ENGINE_OPTIONS:&str = 
"option name Hash type spin default 256 min 1 max 65536
option name Threads type spin default 1 min 1 max 512";

/// UCI controller responsible to read input and command the main engine thread
/// uci implementation inspired by weiawaga/asymptote
pub struct UCIController {
    stop: Arc<AtomicBool>,
    _engine_thread: thread::JoinHandle<()>, // keep handle within scope
    thread_tx: mpsc::Sender<UCICommand>,
}

/// Handles communication with the gui and communicates with main engine thread
impl UCIController {
    pub fn new() -> UCIController {
        let (tx, rx) = mpsc::channel::<UCICommand>();
        let stop = Arc::new(AtomicBool::new(false));

        UCIController {
            stop: stop.clone(),
            _engine_thread: thread::spawn(move || {
                UCIEngine::new(rx, stop).run()
            }),
            thread_tx: tx,
        }
    }

    /// Start UCI I/O loop
    pub fn run(&self) {
        println!("{}", ENGINE_ID);
        println!("{}", ENGINE_OPTIONS);

        let stream = io::stdin().lock();

        for line in stream
                        .lines()
                        .map(|l| l.expect("Parsing error!"))
        {
            match UCICommand::try_from(line.as_ref()) {
                Ok(command) => {
                    match command {
                        UCICommand::Uci => {
                            println!("{}", ENGINE_ID);
                            println!("{}", ENGINE_OPTIONS);
                            println!("uciok");
                        },
                        UCICommand::IsReady => {
                            println!("readyok");
                        },
                        UCICommand::Stop => self.stop.store(true, Ordering::SeqCst), // strict ordering
                        UCICommand::Quit => return,
                        _ => self.thread_tx.send(command).unwrap(),
                    }
                },
                Err(e) => eprintln!("{}", e),
            };
        }
    }
}

/// Enum to represent UCI commands.
enum UCICommand {
    UciNewGame,
    Uci,
    IsReady,
    Position(Board, Vec<String>),
    Go(TimeControl),
    Option(String, String),
    Quit,
    Stop,
}

/// Parse string into uci command
impl TryFrom<&str> for UCICommand {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut tokens = value.split_whitespace();

        match tokens.next() {
            Some("ucinewgame") => Ok(Self::UciNewGame),
            Some("uci") => Ok(Self::Uci),
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
            Some("isready") => Ok(Self::IsReady),
            Some("stop") => Ok(Self::Stop),
            Some("quit") => Ok(Self::Quit),
            Some("go") => Ok(Self::Go(TimeControl::try_from(tokens)?)),
            Some("position") => {
                let board = match tokens.next() {
                    Some("startpos") => Board::default(),
                    Some("fen") => {
                        let fen = &tokens
                            .clone()
                            .take(6)
                            .collect::<Vec<&str>>()
                            .join(" ")
                            [..];
                
                        for _ in 0..6 { tokens.next(); } // advance iterator

                        Board::try_from(fen)?
                    },
                    _ => return Err("Invalid position command"),
                };

                let mut moves = Vec::new();
                if let Some("moves") = tokens.next() {
                    moves = tokens
                        .map(String::from)
                        .collect();
                }

                Ok(Self::Position(board, moves))
            }
            _ => Err("Error parsing command!"),
        }
    }
}