/// Implement structures which allow Carp to communicate via the UCI protocol
/// https://en.wikipedia.org/wiki/Universal_Chess_Interface
use std::{
    io,
    io::BufRead,
    str::FromStr,
    sync::atomic::{AtomicBool, Ordering},
    sync::{Arc, mpsc},
    thread,
};

use crate::{
    clock::TimeControl, position::Position, search_params::P, syzygy::probe::TB,
    thread::ThreadPool, tt::TT,
};
use chess::board::{BULK, NO_BULK};

const NAME: &str = "Carp";
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

const BASE_OPTIONS: &str = "
option name Hash type spin default 16 min 1 max 1048576 
option name Threads type spin default 1 min 1 max 512
option name Minimal type check default false";

#[cfg(feature = "syzygy")]
const SYZYGY_OPTIONS: &str = "
option name SyzygyPath type string default <empty>
option name SyzygyProbeLimit type spin default 6 min 0 max 6";

#[cfg(not(feature = "syzygy"))]
const SYZYGY_OPTIONS: &str = "";

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
    BulkPerft(usize),
    Perft(usize),
    Print,
    Eval,
    NNUEBench,
    PrintParams,
    Wait,
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
            Some("position") => Ok(Self::Position(Box::new(
                tokens.collect::<Vec<&str>>().join(" ").parse()?,
            ))),
            Some("go") => Ok(Self::Go(tokens.collect::<Vec<&str>>().join(" ").parse()?)),
            Some("quit") => Ok(Self::Quit),
            Some("stop") => Ok(Self::Stop),

            Some("bperft") => match tokens.next().ok_or("No option value!")?.parse() {
                Ok(d) if d > 0 => Ok(Self::BulkPerft(d)),
                _ => Err("Could not parse depth!"),
            },
            Some("perft") => match tokens.next().ok_or("No option value!")?.parse() {
                Ok(d) if d > 0 => Ok(Self::Perft(d)),
                _ => Err("Could not parse depth!"),
            },
            Some("print") => Ok(Self::Print),
            Some("eval") => Ok(Self::Eval),
            Some("nnuebench") => Ok(Self::NNUEBench),
            Some("params") => Ok(Self::PrintParams),
            Some("wait") => Ok(Self::Wait),
            _ => Err("Error parsing command!"),
        }
    }
}

/// UCI reader responsible of reading input and forwarding commands to the main controller
/// We keep a global stop flag that we hand out through an reference counted pointer to all search
/// threads, to be able to stop the search upon receiving the stop/quit command.
///
/// implementation inspired by weiawaga/asymptote
pub struct UCIReader {
    stop: Arc<AtomicBool>,
    searching: Arc<AtomicBool>,
    controller_tx: mpsc::Sender<UCICommand>,
    done_rx: mpsc::Receiver<()>,
}

impl Default for UCIReader {
    fn default() -> Self {
        let (controller_tx, controller_rx) = mpsc::channel::<UCICommand>();
        let (done_tx, done_rx) = mpsc::channel::<()>(); // <— NEW

        let stop = Arc::new(AtomicBool::new(false));
        let searching = Arc::new(AtomicBool::new(false));
        let (thread_stop, thread_searching) = (stop.clone(), searching.clone());
        thread::spawn(move || {
            UCIController::run(controller_rx, done_tx, thread_stop, thread_searching)
        });

        Self {
            stop,
            searching,
            controller_tx,
            done_rx,
        }
    }
}

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
                            print!("{BASE_OPTIONS}");
                            println!("{SYZYGY_OPTIONS}");
                            P::print_options();
                            println!("uciok");
                        }
                        UCICommand::IsReady => {
                            println!("readyok");
                        }
                        UCICommand::Stop => self.stop.store(true, Ordering::SeqCst), // strict ordering
                        UCICommand::Wait => {
                            // Block until the controller notifies completion.
                            if self.searching.load(Ordering::SeqCst) {
                                self.done_rx.recv().unwrap();
                            }
                        }
                        UCICommand::Quit => return,
                        UCICommand::Go(_) => {
                            self.searching.store(true, Ordering::SeqCst);
                            self.controller_tx.send(command).unwrap()
                        }
                        _ => self.controller_tx.send(command).unwrap(),
                    }
                }
                Err(e) => eprintln!("{e}"),
            };
        }
    }
}

/// Main runnable controller for the engine, handling search commands.
struct UCIController();

impl UCIController {
    /// Directly handle the "active" uci commands forwarded by the controller.
    /// Meant to be run on a separate thread, to allow for async search interruption.
    fn run(
        controller_rx: mpsc::Receiver<UCICommand>,
        done_tx: mpsc::Sender<()>,
        stop: Arc<AtomicBool>,
        searching: Arc<AtomicBool>,
    ) {
        let mut position = Position::default();
        let mut tt = TT::default();
        let mut tb = TB::default();
        let mut syzygy_probe_limit = TB::MAX_MEN;
        let mut thread_pool = ThreadPool::new(stop);

        for command in &controller_rx {
            match command {
                UCICommand::UciNewGame => {
                    position = Position::default();
                    tt.clear();
                    thread_pool.reset();
                }

                UCICommand::Option(name, value) => match &name[..] {
                    "Hash" => match value.parse::<usize>() {
                        Ok(size) if size > 0 => tt.resize(size),
                        _ => eprintln!("Could not parse hash option value!"),
                    },
                    "Threads" => match value.parse::<usize>() {
                        Ok(size) if size > 0 => thread_pool.resize(size - 1),
                        _ => eprintln!("Could not parse threads option value!"),
                    },
                    "Minimal" => match value.as_str() {
                        "true" => thread_pool.minimal_output = true,
                        "false" => thread_pool.minimal_output = false,
                        _ => eprintln!("Could not parse minimal option value!"),
                    },
                    "SyzygyPath" => tb.activate(&value, syzygy_probe_limit),
                    "SyzygyProbeLimit" => match value.parse::<u8>() {
                        Ok(limit) if limit <= TB::MAX_MEN => {
                            syzygy_probe_limit = limit;
                            tb.n_men = limit;
                        }
                        _ => eprintln!("Could not parse syzygy probe limit option value!"),
                    },
                    _ => {
                        #[cfg(feature = "tune")]
                        P::set_param(name, value);

                        #[cfg(not(feature = "tune"))]
                        eprintln!("Unsupported option command!");
                    }
                },

                UCICommand::Position(pos) => {
                    position = *pos;
                }

                UCICommand::Go(tc) => {
                    tt.increment_age();
                    println!(
                        "bestmove {}",
                        thread_pool.deploy_search(&mut position, &tt, tb, tc),
                    );
                    searching.store(false, Ordering::SeqCst);
                    done_tx.send(()).unwrap();
                }

                UCICommand::BulkPerft(d) => {
                    position.board.perft::<BULK>(d);
                }

                UCICommand::Perft(d) => {
                    position.board.perft::<NO_BULK>(d);
                }

                UCICommand::Print => {
                    println!("{}", position.board);
                }

                UCICommand::Eval => {
                    println!("Static evaluation: {}", position.evaluate());
                }

                UCICommand::NNUEBench => {
                    println!("Eval time: {:.3}ns", position.nnuebench());
                }

                UCICommand::PrintParams => {
                    P::print_params_ob();
                }

                _ => eprintln!("Unexpected UCI command!"),
            }
        }
    }
}
