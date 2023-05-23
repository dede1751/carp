/// Handles time management for iterative deepening
/// Time control implementation closely follows Weiawaga
use std::str::{FromStr, SplitWhitespace};

use std::time::{Duration, Instant};
use std::{
    sync::atomic::{AtomicBool, Ordering},
    sync::Arc,
};

#[derive(Clone, Debug)]
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
                "infinite" => return Ok(TimeControl::Infinite),
                "depth" => return Ok(TimeControl::FixedDepth(parse_value(&mut tokens)?)),
                "nodes" => return Ok(TimeControl::FixedNodes(parse_value(&mut tokens)?)),
                "movetime" => return Ok(TimeControl::FixedTime(parse_value(&mut tokens)?)),
                "wtime" => wtime = Some(parse_value::<i64>(&mut tokens)?.max(0) as u64), // handle negative values
                "btime" => btime = Some(parse_value::<i64>(&mut tokens)?.max(0) as u64),
                "winc" => winc = Some(parse_value(&mut tokens)?),
                "binc" => binc = Some(parse_value(&mut tokens)?),
                "movestogo" => movestogo = Some(parse_value(&mut tokens)?),
                _ => return Err("Incorrect time control!"),
            }
        }

        if let (Some(wtime), Some(btime)) = (wtime, btime) {
            Ok(TimeControl::Variable {
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

// number of nodes between clock checks
const CHECK_FREQUENCY: u64 = 2048;
const OVERHEAD: u64 = 5;

#[derive(Clone, Debug)]
pub struct Clock {
    stop: Arc<AtomicBool>,
    time_control: TimeControl,
    start_time: Instant,
    opt_time: Duration,
    max_time: Duration,
    check_count: u64,
}

impl Clock {
    pub fn new(time_control: TimeControl, stop: Arc<AtomicBool>, white_to_move: bool) -> Clock {
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
                    (opt_time, (5. * opt_time).min(eight))
                } else {
                    let total = ((time / 20) + (inc / 2)) as f64;

                    (0.6 * total, (2. * total).min(time as f64))
                };

                (
                    Duration::from_millis(opt as u64),
                    Duration::from_millis(max as u64),
                )
            }
            _ => (Duration::ZERO, Duration::ZERO),
        };

        Clock {
            stop,
            time_control,
            start_time: Instant::now(),
            opt_time,
            max_time,
            check_count: 0,
        }
    }

    /// Returns a child clock with the same stop flag,to be given to parallel threads
    pub fn get_child_clock(&self) -> Clock {
        Clock {
            stop: self.stop.clone(),
            time_control: TimeControl::Infinite,
            start_time: Instant::now(),
            opt_time: Duration::ZERO,
            max_time: Duration::ZERO,
            check_count: 0,
        }
    }

    /// Returns time elapsed from clock start
    pub fn elapsed(&self) -> Duration {
        self.start_time.elapsed()
    }

    /// Checks whether there is any time to begin the search
    /// This should only every be called before beginning a search.
    pub fn no_search_time(&self) -> bool {
        match self.time_control {
            TimeControl::FixedTime(_) | TimeControl::Variable { .. } => {
                self.opt_time == Duration::ZERO
            }
            _ => false,
        }
    }

    /// Checks whether to deepen the search (true -> continue deepening)
    pub fn start_check(&mut self, depth: usize, nodes: u64) -> bool {
        if self.stop.load(Ordering::SeqCst) {
            return false;
        }

        // at least depth 1
        if depth == 1 {
            return true;
        }

        let start = match self.time_control {
            TimeControl::FixedDepth(d) => depth <= d,
            TimeControl::FixedNodes(n) => nodes <= n,
            TimeControl::FixedTime(_) | TimeControl::Variable { .. } => {
                self.start_time.elapsed() < self.opt_time
            }
            _ => true, // Infinite tc does not depend on start check
        };

        // global stop
        if !start {
            self.stop.store(true, Ordering::SeqCst);
        }

        start
    }

    /// Checks whether to continue searching during the search (true -> continue searching)
    pub fn mid_check(&mut self) -> bool {
        self.check_count += 1;

        // load atomic value only every CHECK_FREQUENCY checks
        if self.check_count % CHECK_FREQUENCY == 0 && self.stop.load(Ordering::SeqCst) {
            return false;
        }

        let proceed = match self.time_control {
            TimeControl::FixedTime(_) | TimeControl::Variable { .. } => {
                // check elapsed time only every CHECK_FREQUENCY checks
                if self.check_count % CHECK_FREQUENCY == 0 {
                    self.start_time.elapsed() < self.max_time
                } else {
                    true
                }
            }
            _ => true,
        };

        // global stop
        if !proceed {
            self.stop.store(true, Ordering::SeqCst);
        }

        proceed
    }
}
