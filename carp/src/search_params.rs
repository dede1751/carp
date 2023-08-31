/// Search parameters are kept in this module.
/// Terminology:
///     THRESHOLD: anything takes effect AFTER this amount or BELOW this depth (always <=)
///     LOWER_LIMIT: nothing takes effect BELOW this depth (always >=)
///     BASE: flat bonus in a formula
///     MARGIN: multiplicative (usually depth) coefficient in a formula
///     FACTOR: dividing coefficient in a formula
pub use chess::params::*;
use std::mem::transmute;

pub const INFINITY: Eval = 32001; // score upper bound
pub const MATE: Eval = 32000; // mate in 0 moves
pub const MATE_IN_PLY: Eval = MATE - MAX_DEPTH as Eval; // mate in x moves
pub const TB_MATE: Eval = 31000; // tb win in 0 moves
pub const TB_MATE_IN_PLY: Eval = TB_MATE - MAX_DEPTH as Eval; // tb win in x moves

pub const HIST_MAX: i32 = 8192;
pub const CONT_HIST_MAX: i32 = 16384;
pub const HISTORY_MAX: i32 = HIST_MAX + 2 * CONT_HIST_MAX;

pub const TT_REPLACE_OFFSET: usize = 11;

pub const ASPIRATION_LOWER_LIMIT: usize = 5;
pub const ASPIRATION_WINDOW: Eval = 25;
pub const BIG_DELTA: Eval = 1100;

pub const LMR_THRESHOLD: usize = 2;
pub const LMR_LOWER_LIMIT: usize = 2;
const LMR_TABLE: [[usize; 64]; 64] = unsafe { transmute(*include_bytes!("../../bins/lmr.bin")) };

pub fn lmr_reduction(depth: usize, move_count: usize) -> usize {
    LMR_TABLE[depth.min(63)][move_count.min(63)]
}

pub const SE_LOWER_LIMIT: usize = 8;

pub const RFP_THRESHOLD: usize = 8;
pub const RFP_MARGIN: Eval = 80;
pub const RFP_IMPROVING_MARGIN: Eval = 55;

pub const NMP_LOWER_LIMIT: usize = 3;
pub const NMP_IMPROVING_MARGIN: Eval = 70;
pub const NMP_BASE: usize = 4;
pub const NMP_FACTOR: usize = 4;

pub const IIR_LOWER_LIMIT: usize = 4;

pub const HLP_THRESHOLD: usize = 2;
pub const HLP_BASE: i32 = -5000;

pub const EFP_THRESHOLD: usize = 5;
pub const EFP_BASE: Eval = 80;
pub const EFP_MARGIN: Eval = 90;

pub const LMP_THRESHOLD: usize = 8;
pub const LMP_BASE: usize = 4;

pub const SEE_PRUNING_THRESHOLD: usize = 9;
pub const SEE_CAPTURE_MARGIN: Eval = -20;
pub const SEE_QUIET_MARGIN: Eval = -65;

pub const PIECE_VALUES: [Eval; 12] = [161, 161, 446, 446, 464, 464, 705, 705, 1322, 1322, 0, 0];
