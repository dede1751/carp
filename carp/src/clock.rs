/// Handles time management for iterative deepening and async search.
use std::str::{FromStr, SplitWhitespace};
use std::time::{Duration, Instant};
use std::{
    sync::atomic::{AtomicBool, AtomicU64, Ordering},
    sync::Arc,
};

use chess::{moves::Move, square::Square};

/// Time Controls supported by the UCI protocol.
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug)]
pub enum TimeControl {
    Infinite,
    FixedDepth(usize),
    FixedNodes(u64),
    FixedTime(u64),
    Variable {
        wtime: u64,
        btime: u64,
        winc: Option<u64>,
        binc: Option<u64>,
        movestogo: Option<u64>,
    },
}

fn parse_value<T: FromStr>(tokens: &mut SplitWhitespace) -> Result<T, &'static str> {
    tokens
        .next()
        .ok_or("Missing value for timecontrol!")?
        .parse()
        .or(Err("Unable to parse!"))
}

/// Convert input to correct time control
impl FromStr for TimeControl {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();
        let mut wtime: Option<u64> = None;
        let mut btime: Option<u64> = None;
        let mut winc: Option<u64> = None;
        let mut binc: Option<u64> = None;
        let mut movestogo: Option<u64> = None;

        while let Some(token) = tokens.next() {
            // needed to be able to pass tokens to parse_value
            match token {
                "infinite" => return Ok(Self::Infinite),
                "depth" => return Ok(Self::FixedDepth(parse_value(&mut tokens)?)),
                "nodes" => return Ok(Self::FixedNodes(parse_value(&mut tokens)?)),
                "movetime" => return Ok(Self::FixedTime(parse_value(&mut tokens)?)),
                "wtime" => wtime = Some(parse_value::<i64>(&mut tokens)?.max(0) as u64), // handle negative values
                "btime" => btime = Some(parse_value::<i64>(&mut tokens)?.max(0) as u64),
                "winc" => winc = Some(parse_value(&mut tokens)?),
                "binc" => binc = Some(parse_value(&mut tokens)?),
                "movestogo" => movestogo = Some(parse_value(&mut tokens)?),
                _ => return Err("Incorrect time control!"),
            }
        }

        if let (Some(wtime), Some(btime)) = (wtime, btime) {
            Ok(Self::Variable {
                wtime,
                btime,
                winc,
                binc,
                movestogo,
            })
        } else {
            Err("Missing variable time control values!")
        }
    }
}

const CHECK_FREQUENCY: u64 = 2048; // Nodes between checking time/atomic access
const OVERHEAD: u64 = 50;

/// Clocks handle time management during search.
/// Contains async counters used to synchronize time management/node counting across threads.
#[derive(Clone, Debug)]
pub struct Clock {
    time_control: TimeControl,

    // Global flags for SMP
    global_stop: Arc<AtomicBool>,
    pub last_nodes: u64,
    global_nodes: Arc<AtomicU64>,

    // Base timers
    start_time: Instant,
    opt_time: Duration,
    max_time: Duration,

    // Time scaling factors
    prev_best: Move,
    stability: u32,
    node_count: [[u64; Square::COUNT]; Square::COUNT],
}

impl Clock {
    /// Init a new clock for the given timecontrol.
    /// Only meant to be used by the main thread in SMP.
    pub fn new(
        global_stop: Arc<AtomicBool>,
        global_nodes: Arc<AtomicU64>,
        time_control: TimeControl,
        white_to_move: bool,
    ) -> Self {
        let (opt_time, max_time) = match time_control {
            TimeControl::FixedTime(time) => (
                Duration::from_millis(time - OVERHEAD.min(time)),
                Duration::from_millis(time - OVERHEAD.min(time)),
            ),
            TimeControl::Variable {
                wtime,
                btime,
                winc,
                binc,
                movestogo,
            } => {
                let (time, inc) = if white_to_move {
                    match winc {
                        Some(inc) => (wtime, inc),
                        None => (wtime, 0),
                    }
                } else {
                    match binc {
                        Some(inc) => (btime, inc),
                        None => (btime, 0),
                    }
                };

                // When below overhead, make opt and max time 0
                let time = time - OVERHEAD.min(time);
                let inc = if time < OVERHEAD { 0 } else { inc };

                // This time allocation formula is taken from Svart by Crippa
                let (opt, max) = if let Some(moves) = movestogo {
                    let scale = 0.7 / (moves.min(50) as f64);
                    let eight = 0.8 * time as f64;

                    let opt_time = (scale * time as f64).min(eight);
                    (opt_time, (5.0 * opt_time).min(eight))
                } else {
                    let total = ((time / 20) + (inc * 3 / 4)) as f64;

                    (0.6 * total, (2.0 * total).min(time as f64))
                };

                (
                    Duration::from_millis(opt as u64),
                    Duration::from_millis(max as u64),
                )
            }
            _ => (Duration::ZERO, Duration::ZERO),
        };

        Self {
            time_control,
            global_stop,
            last_nodes: 0,
            global_nodes,
            start_time: Instant::now(),
            opt_time,
            max_time,
            prev_best: Move::NULL,
            stability: 0,
            node_count: [[0; Square::COUNT]; Square::COUNT],
        }
    }

    /// Initialize a spinner clock (used as a placeholder or for SMP workers)
    pub fn spin_clock(global_stop: Arc<AtomicBool>, global_nodes: Arc<AtomicU64>) -> Self {
        Self::new(global_stop, global_nodes, TimeControl::Infinite, false)
    }

    /// Returns the global node count across threads.
    pub fn global_nodes(&self) -> u64 {
        self.global_nodes.load(Ordering::SeqCst)
    }

    /// Returns time elapsed from clock start.
    pub fn elapsed(&self) -> Duration {
        self.start_time.elapsed()
    }

    /// Checks whether there is any time to begin the search
    /// This should only ever be called before beginning a search.
    pub fn no_search_time(&self) -> bool {
        match self.time_control {
            TimeControl::FixedTime(_) | TimeControl::Variable { .. } => {
                self.opt_time == Duration::ZERO
            }
            _ => false,
        }
    }

    /// Update the number of nodes searched by a single move.
    /// Only called from the root to see how deep each move has been searched.
    pub fn update_node_counts(&mut self, m: Move, delta: u64) {
        self.node_count[m.get_src() as usize][m.get_tgt() as usize] += delta;
    }

    /// Checks whether to deepen the search.
    /// Information passed to this function should be thread-local.
    pub fn start_search(&mut self, depth: usize, nodes: u64, best_move: Move) -> bool {
        if self.global_stop.load(Ordering::SeqCst) {
            return false;
        }

        // at least depth 1
        if depth == 1 {
            return true;
        }

        let start = match self.time_control {
            TimeControl::FixedDepth(d) => depth <= d,
            TimeControl::FixedNodes(n) => self.global_nodes() <= n,
            TimeControl::FixedTime(_) | TimeControl::Variable { .. } => {
                let opt_scale = if nodes != 0 {
                    // Node Count scaling: the more nodes dedicated to the best move, the less time
                    // we allocate.
                    // Scale factor from Ethereal, scale between 50% and 240%
                    let bm_nodes =
                        self.node_count[best_move.get_src() as usize][best_move.get_tgt() as usize];
                    let bm_fraction = bm_nodes as f64 / nodes as f64;
                    let bm_factor = (0.4 + (1.0 - bm_fraction) * 2.0).max(0.5);

                    // Best MoveStability scaling: the more stable the best move is, the less time
                    // we allocate. Tactical moves become "stable" faster because they are more
                    // likely to be forced.
                    // Scale factor from Berserk.
                    let prev_stability = self.stability;
                    if self.prev_best == best_move {
                        let stab_inc = 1 + u32::from(!best_move.get_type().is_quiet());
                        self.stability = (self.stability + stab_inc).min(10)
                    };

                    let stability_factor = if prev_stability == 10 && self.stability == 0 {
                        1.5
                    } else {
                        1.25 - (self.stability as f64 * 0.05)
                    };

                    bm_factor * stability_factor
                } else {
                    1.0
                };

                self.elapsed() < self.opt_time.mul_f64(opt_scale)
            }
            _ => true, // Infinite tc does not depend on start check
        };

        if !start {
            self.global_stop.store(true, Ordering::SeqCst);
        }

        start
    }

    /// Checks whether to halt an ongoing search.
    /// Only loads/stores atomics and checks the time every CHECK_FREQUENCY nodes.
    pub fn continue_search(&mut self, nodes: u64) -> bool {
        let searched = nodes - self.last_nodes;

        if searched >= CHECK_FREQUENCY {
            self.global_nodes.fetch_add(searched, Ordering::SeqCst);
            self.last_nodes = nodes;

            if self.global_stop.load(Ordering::SeqCst) {
                return false;
            }
        }

        let proceed = match self.time_control {
            TimeControl::FixedTime(_) | TimeControl::Variable { .. } => {
                searched < CHECK_FREQUENCY || self.elapsed() < self.max_time
            }
            _ => true,
        };

        if !proceed {
            self.global_stop.store(true, Ordering::SeqCst);
        }

        proceed
    }
}
