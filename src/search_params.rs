/// Save search parameters in a single module
/// Used to simplify engine testing and future tuning.

pub type Eval = i16;
pub const MAX: Eval = 30000; // score upper bound
pub const MAX_DEPTH: usize = 128; // max depth to search at

/// Returns true if the opponent is checkmated
pub fn is_mate(eval: Eval) -> bool {
    ((MAX - MAX_DEPTH as i16)..MAX).contains(&eval)
}

pub const HISTORY_LOWER_LIMIT: usize = 3; // minimum depth at which history updates happen

pub const LMR_THRESHOLD: usize = 2; // moves to execute before any reduction
pub const LMR_LOWER_LIMIT: usize = 2; // stop applying lmr near leaves
pub const LMR_BASE: f32 = 0.75; // increase to reduce every move more
pub const LMR_FACTOR: f32 = 2.0; // increase to reduce less further in the movelist

pub const RFP_THRESHOLD: usize = 8; // depth at which rfp kicks in
pub const RFP_MARGIN: Eval = 130; // multiplier for eval safety margin for rfp cutoffs

pub const NMP_LOWER_LIMIT: usize = 3; // stop applying nmp near leaves
pub const NMP_BASE_R: usize = 4; // null move pruning reduced depth
pub const NMP_FACTOR: usize = 4; // increase to reduce more at higher depths

pub const IIR_LOWER_LIMIT: usize = 4; // stop applying iir near leaves

pub const HLP_THRESHOLD: usize = 2; // depth at which history leaf pruning kicks in
pub const HLP_MARGIN: i32 = 0; // increase to prune quiets with higher history scores

pub const EFP_THRESHOLD: usize = 8; // depth at which extended futility pruning kicks in
pub const EFP_BASE: Eval = 100; // base eval bonus margin for efp
pub const EFP_MARGIN: Eval = 120; // multiplier for eval bonus margin for efp

pub const LMP_THRESHOLD: usize = 8; // depth at which late move pruning kicks in
pub const LMP_BASE: usize = 4; // lmp move count at depth = 0

pub const ASPIRATION_THRESHOLD: usize = 4; // depth at which windows are reduced
pub const ASPIRATION_WINDOW: Eval = 50; // aspiration window width

pub const QS_DELTA_MARGIN: Eval = 1100; // highest queen value possible
pub const QS_FUTILITY_MARGIN: Eval = 200; // overhead we allow for captures in qs
pub const QS_PIECE_VALUES: [Eval; 6] = [161, 446, 464, 705, 1322, 0]; // qs fp piece values
