/// Save search parameters in a single module
/// Used to simplify engine testing and future tuning.

pub type Eval = i32;
pub const MAX_DEPTH: usize = 127; // max depth to search at
pub const INFINITY: Eval = 32001; // score upper bound
pub const MATE: Eval = 32000; // mate in 0 moves
pub const MATE_IN_PLY: Eval = MATE - MAX_DEPTH as Eval; // mate in x moves

pub const HISTORY_LOWER_LIMIT: usize = 3; // minimum depth at which history updates happen

pub const LMR_THRESHOLD: usize = 2; // moves to execute before any reduction
pub const LMR_LOWER_LIMIT: usize = 2; // stop applying lmr near leaves
pub const LMR_BASE: f32 = 0.75; // increase to reduce every move more
pub const LMR_FACTOR: f32 = 2.0; // increase to reduce less further in the movelist

pub const SE_LOWER_LIMIT: usize = 8; // stop applying SE near leaves

pub const RFP_THRESHOLD: usize = 8; // depth at which rfp kicks in
pub const RFP_MARGIN: Eval = 80; // multiplier for eval safety margin for rfp cutoffs
pub const RFP_IMPROVING_MARGIN: Eval = 55; // multiplier for improving flag

pub const NMP_LOWER_LIMIT: usize = 3; // stop applying nmp near leaves
pub const NMP_IMPROVING_MARGIN: Eval = 70; // multiplier for improving flag
pub const NMP_BASE_R: usize = 4; // null move pruning reduced depth
pub const NMP_FACTOR: usize = 4; // increase to reduce more at higher depths

pub const IIR_LOWER_LIMIT: usize = 4; // stop applying iir near leaves

pub const HLP_THRESHOLD: usize = 2; // depth at which history leaf pruning kicks in
pub const HLP_MARGIN: i32 = -5000; // increase to prune quiets with higher history scores

pub const EFP_THRESHOLD: usize = 5; // depth at which extended futility pruning kicks in
pub const EFP_BASE: Eval = 80; // base eval bonus margin for efp
pub const EFP_MARGIN: Eval = 90; // multiplier for eval bonus margin for efp

pub const LMP_THRESHOLD: usize = 8; // depth at which late move pruning kicks in
pub const LMP_BASE: usize = 4; // lmp move count at depth = 0

pub const SEE_PRUNING_THRESHOLD: usize = 9; // depth at which see pruning kicks in
pub const SEE_CAPTURE_MARGIN: Eval = -20; // threshold for see pruning for captures
pub const SEE_QUIET_MARGIN: Eval = -65; // threshold for see pruning for quiets

pub const ASPIRATION_THRESHOLD: usize = 5; // depth at which windows are reduced
pub const ASPIRATION_WINDOW: Eval = 25; // aspiration window width

pub const QS_FUTILITY_MARGIN: Eval = 200; // overhead we allow for captures in qs
pub const BIG_DELTA: Eval = 1100; // highest queen value possible

pub const PIECE_VALUES: [Eval; 12] = [
    // piece values for static evaluations
    161, 161, 446, 446, 464, 464, 705, 705, 1322, 1322, 0, 0,
];
