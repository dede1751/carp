
/// UCI Chess engine
/// 
/// Sets up positions and dispatches searches. The search itself is responsible for the stop
/// command, which is both used by the time control and the uci controller.
/// Lazy SMP is implemented in the simplest way possible: threads share only the same TT, only
/// the main thread controls the actual clock while the others use an infinite time control.
/// All best moves are collected, and the deepest with most occurrences will be returned.
use std::{ 
    thread,
    sync::{ Arc, mpsc },
    sync::atomic::{ AtomicBool, Ordering },
};

use super::UCICommand;
use crate::{
    board_repr::Board,
    piece::Color,
    moves::Move,
    tables::Tables,
    search::Search,
    tt::{TT, DEFAULT_SIZE},
    clock::{ Clock, TimeControl },
    zobrist::ZHash,
};

pub(super) struct UCIEngine {
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
            tt_size: DEFAULT_SIZE,
            controller_rx: rx,
            stop,
            worker_count: 0
        }
    }
    
    /// Dispatch main engine thread.
    /// Handles the "active" uci commands forwarded by the controller and dispatches helpers.
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