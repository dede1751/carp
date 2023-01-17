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
    FixedDepth(u8),
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
    Ok(tokens
        .next()
        .ok_or("Missing value for timecontrol!")?
        .parse()
        .or(Err("Unable to parse!"))?)
}

/// Convert input to correct time control
impl TryFrom<SplitWhitespace<'_>> for TimeControl {
    type Error = &'static str;

    fn try_from(mut tokens: SplitWhitespace) -> Result<Self, Self::Error> {
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
                "wtime" => wtime = Some(parse_value(&mut tokens)?),
                "btime" => btime = Some(parse_value(&mut tokens)?),
                "winc" => winc = Some(parse_value(&mut tokens)?),
                "binc" => binc = Some(parse_value(&mut tokens)?),
                "movestogo" => movestogo = Some(parse_value(&mut tokens)?),
                _ => return Err("Incorrect time control!"),
            }
        }

        if wtime.is_none() || btime.is_none() {
            Err("Missing variable time control values!")
        } else {
            Ok(TimeControl::Variable {
                wtime: wtime.unwrap(),
                btime: btime.unwrap(),
                winc,
                binc,
                movestogo,
            })
        }
    }
}

// number of nodes between clock checks
const CHECK_FREQUENCY: u64 = 2048;

#[derive(Clone, Debug)]
pub struct Clock {
    stop: Arc<AtomicBool>,
    time_control: TimeControl,
    start_time: Instant,
    end_time: Duration,
    check_count: u64,
}

impl Clock {
    pub fn new(time_control: TimeControl, stop: Arc<AtomicBool>, white_to_move: bool) -> Clock {
        let end_time: Duration = match time_control {
            TimeControl::FixedTime(time) => Duration::from_millis(time),
            TimeControl::Variable {
                wtime,
                btime,
                winc,
                binc,
                movestogo,
            } => {
                let (time, increment) = if white_to_move {
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

                // divide current time by movestogo, if not provided default to 35.
                Duration::from_millis(time / movestogo.unwrap_or(35) + increment)
            }
            _ => Duration::ZERO,
        };

        Clock {
            stop,
            time_control,
            start_time: Instant::now(),
            end_time,
            check_count: 0,
        }
    }

    /// Checks whether to deepen the search (true -> continue deepening)
    pub fn start_check(&mut self, depth: u8) -> bool {
        if self.stop.load(Ordering::SeqCst) {
            return false;
        }

        // at least depth 1
        if depth == 1 {
            return true;
        }

        let start = match self.time_control {
            TimeControl::FixedDepth(d) => depth <= d,
            TimeControl::FixedTime(_) | TimeControl::Variable { .. } => {
                let time = self.start_time.elapsed();

                // the deepest search takes the majority of the time, need at least half the full time
                time < (self.end_time / 2)
            }
            _ => true, // FixedNodes/Infinite do not depend on start check
        };

        // global stop
        if !start {
            self.stop.store(true, Ordering::SeqCst);
        }

        start
    }

    /// Checks whether to continue searching during the search (true -> continue searching)
    pub fn mid_check(&mut self, nodes: u64) -> bool {
        self.check_count += 1;

        // load atomic value only every CHECK_FREQUENCY checks
        if self.check_count % CHECK_FREQUENCY == 0 && self.stop.load(Ordering::SeqCst) {
            return false;
        }

        let proceed = match self.time_control {
            TimeControl::FixedNodes(n) => n <= nodes,
            TimeControl::FixedTime(_) | TimeControl::Variable { .. } => {
                // check elapsed time only every CHECK_FREQUENCY checks
                if self.check_count % CHECK_FREQUENCY == 0 {
                    self.start_time.elapsed() < self.end_time
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
