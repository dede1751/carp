/// UCI integration
use std::{ 
    io,
    io::BufRead, 
    thread,
    sync::{ Arc, mpsc },
    sync::atomic::{ AtomicBool, Ordering },
};

use crate::{
    board_repr::Board,
    piece::Color,
    moves::Move,
    tables::Tables,
    search::Search,
    tt::TT,
    clock::{ Clock, TimeControl },
    zobrist::ZHash,
};

const ENGINE_ID: &str = 
"id name Carp 0.1
id author Andrea S.";

const ENGINE_OPTIONS:&str = 
"option name Hash type spin default 256 min 1 max 65536
option name Threads type spin default 1 min 1 max 512";

// uci implementation inspired by weiawaga/asymptote
pub struct UCIController {
    stop: Arc<AtomicBool>,
    _engine_thread: thread::JoinHandle<()>,
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

/// # UCI Chess engine
/// 
/// Sets up positions and dispatches searches. The search itself is responsible for the stop
/// command, which is both used by the time control and the uci controller.
/// Lazy SMP is implemented in the most simple way possible: threads share only the same TT, only
/// the main thread controls the actual clock while the others use an infinite time control.
/// All best moves are collected, and the deepest with most occurrences will be returned.
struct UCIEngine {
    board: Board,
    history: Vec<ZHash>,
    tt: TT,
    tt_size: usize,
    controller_rx: mpsc::Receiver<UCICommand>,
    stop: Arc<AtomicBool>,
    worker_count: usize,
}

impl UCIEngine {
    pub fn new(
        rx: mpsc::Receiver<UCICommand>,
        stop: Arc<AtomicBool>,
    ) -> UCIEngine {
        UCIEngine{
            board: Board::default(),
            history: Vec::new(),
            tt: TT::default(),
            tt_size: 256,
            controller_rx: rx,
            stop,
            worker_count: 0
        }
    }
    
    pub fn run(&mut self) {
        let tables = Tables::default();

        for command in &self.controller_rx {
            match command {
                UCICommand::UciNewGame => {
                    self.board = Board::default();
                    self.tt = TT::new(self.tt_size);
                }

                UCICommand::Position(board, moves) => {
                    self.history = vec![board.hash];
                    self.board = board;

                    for move_string in moves {
                        let new = self.board
                            .generate_moves(&tables)
                            .into_iter()
                            .find(|m| { move_string == m.to_string()});

                        match new {
                            Some(new_move) => {
                                match self.board.make_move(new_move, &tables) {
                                    Some(b) => {
                                        self.board = b;
                                        self.history.push(b.hash);
                                    }
                                    None => eprintln!("Move is not legal!"),
                                }
                            },
                            None => eprintln!("Move is not pseudolegal!"),
                        };
                    }
                }

                UCICommand::Go(tc) => {
                    self.stop.store(false, Ordering::Relaxed);

                    let best_move = thread::scope(|scope|{
                        let mut worker_handles = Vec::with_capacity(self.worker_count);
                        let mut results = Vec::with_capacity(self.worker_count);

                        // Start making main search with master clock
                        let mut main_search = Search::new(
                            self.history.clone(),
                            Clock::new(tc, self.stop.clone(), self.board.side == Color::White),
                            &self.tt,
                            &tables
                        );

                        // Deploy all worker search threads.
                        for _ in 1..self.worker_count {
                            let worker_board: Board = self.board.clone();
                
                            let mut worker_search = Search::new(
                                self.history.clone(),
                                Clock::new(
                                    TimeControl::Infinite,
                                    self.stop.clone(),
                                    true
                                ),
                                &self.tt,
                                &tables
                            );

                            worker_handles.push(
                                scope.spawn(move || 
                                    worker_search.iterative_search(worker_board, false)
                                )
                            );
                        };

                        // Deploy main search
                        results.push(
                            main_search.iterative_search(self.board.clone(), true)
                        );

                        for handle in worker_handles {
                            match handle.join() {
                                Ok(result) => results.push(result),
                                Err(e) => eprintln!("{:?}", e),
                            }
                        }

                        results.sort_by(|(_, a), (_, b)| b.cmp(a) );
                        let highest_depth = results[0].1;

                        results.into_iter()
                            .filter_map(|(m, d)| {
                                if d == highest_depth { Some(m) }
                                else { None }
                            })
                            .fold(std::collections::HashMap::<Move, u8>::new(), |mut map, x| {
                                *map.entry(x).or_default() += 1;
                                map
                            })
                            .into_iter()
                            .max_by_key(|(_, value)| *value)
                            .map(|(m, _)| m)
                            .unwrap() // always at least one search, impossible panic
                    });

                    println!("\nbestmove {}", best_move);
                }

                UCICommand::Option(name, value) => {
                    match &name[..] {
                        "Hash" => {
                            match value.parse() {
                                Ok(size) => {
                                    self.tt_size = size;
                                    self.tt = TT::new(size);
                                }
                                Err(_) => eprintln!("Could not parse hash option value!")
                            }
                        }
                        "Threads" => {
                            match value.parse::<usize>() {
                                Ok(size) => {
                                    self.worker_count = size;
                                }
                                Err(_) => eprintln!("Could not parse threads option value!")
                            }
                        }
                        _ => eprintln!("Unsupported option command!")
                    }
                }

                _ => eprintln!("Unexpected UCI command!"),
            }
        }
    }
}