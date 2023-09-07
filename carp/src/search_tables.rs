/// Implements various tables used within the search:
///    - History Tables: used for ordering quiet moves
///    - PV Table: holds the principal variation, which is the main line the engine predicts
use crate::search_params::*;
use chess::{
    board::Board,
    moves::Move,
    piece::{Color, Piece},
    square::Square,
};

/// PV Tables store the principal variation.
/// Whenever a move scores within the window, it is added to the PV table of its child subtree.
#[derive(Clone, Debug)]
pub struct PVTable {
    pub length: usize,
    pub moves: [Move; MAX_DEPTH],
}

impl Default for PVTable {
    fn default() -> Self {
        Self {
            length: 0,
            moves: [Move::NULL; MAX_DEPTH],
        }
    }
}

impl std::fmt::Display for PVTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::from("pv");

        for m in &self.moves[0..self.length] {
            s.push_str(&format!(" {m}"));
        }

        write!(f, "{}", s)
    }
}

impl PVTable {
    /// Extend a shallower PV line with a new move, overwriting the old line.
    pub fn update_pv_line(&mut self, m: Move, old: &Self) {
        self.length = old.length + 1;
        self.moves[0] = m;
        self.moves[1..=old.length].copy_from_slice(&old.moves[..old.length]);
    }
}

type History = [[[i16; Square::COUNT]; Square::COUNT]; 2];
type ContinuationHistory = [[[[i16; Square::COUNT]; Square::COUNT]; Square::COUNT]; Piece::TOTAL];
type CaptureHistory = [[[i16; Piece::COUNT - 1]; Square::COUNT]; Piece::TOTAL];

/// History bonus is Stockfish's "gravity"
pub fn history_bonus(depth: usize) -> i16 {
    HISTORY_MAX_BONUS.min(HISTORY_FACTOR * depth as i16 - HISTORY_OFFSET)
}

/// Taper history so that it's bounded to +-MAX
/// This keeps us within i16 bounds.
/// Discussed here:
/// http://www.talkchess.com/forum3/viewtopic.php?f=7&t=76540
const fn taper_bonus<const MAX: i32>(bonus: i16, old: i16) -> i16 {
    let o = old as i32;
    let b = bonus as i32;

    // Use i32's to avoid overflows
    (o + b - (o * b.abs()) / MAX) as i16
}

/// Simple history tables are used for standard move histories.
///     Indexing: [side][src][tgt]
#[derive(Clone, Debug)]
pub struct HistoryTable<const MAX: i32> {
    history: History,
}

impl<const MAX: i32> Default for HistoryTable<MAX> {
    fn default() -> Self {
        Self {
            history: [[[0; Square::COUNT]; Square::COUNT]; 2],
        }
    }
}

impl<const MAX: i32> HistoryTable<MAX> {
    /// Get an index for the given move.
    fn index(m: Move, side: Color) -> (usize, usize, usize) {
        (side as usize, m.get_src() as usize, m.get_tgt() as usize)
    }

    /// Add a history bonus value to the given move.
    fn add_bonus(&mut self, bonus: i16, m: Move, side: Color) {
        let index = Self::index(m, side);
        let old = &mut self.history[index.0][index.1][index.2];

        *old = taper_bonus::<MAX>(bonus, *old);
    }

    /// Update the history table after a beta cutoff.
    /// Gives a positive bonus to the fail-high move and a negative bonus to all other moves tried.
    pub fn update(&mut self, bonus: i16, curr: Move, side: Color, searched: &Vec<Move>) {
        for m in searched {
            self.add_bonus(-bonus, *m, side);
        }
        self.add_bonus(bonus, curr, side);
    }

    /// Get the history score for a given move by the given side.
    pub fn get_score(&self, m: Move, side: Color) -> i32 {
        let index = Self::index(m, side);

        self.history[index.0][index.1][index.2] as i32
    }
}

/// Continuation history tables are used for counter moves and followup moves.
/// The first two indices are taken from the history move, the last two from the current move.
///    - Counter Move: the previous move by the opponent
///    - Followup Move: our previous move
///
///     Indexing: [old_piece][old_tgt][new_src][new_tgt]
#[derive(Clone, Debug)]
pub struct ContinuationHistoryTable<const MAX: i32> {
    history: Box<ContinuationHistory>,
}

/// Used to box arrays without blowing the stack on debug builds.
/// Warning: wildly unsafe behavior for non-zeroable types
/// All credits go to Cosmo, creator of Viridithas
fn box_array<T>() -> Box<T> {
    unsafe {
        let layout = std::alloc::Layout::new::<T>();
        let ptr = std::alloc::alloc_zeroed(layout);
        if ptr.is_null() {
            std::alloc::handle_alloc_error(layout);
        }
        Box::from_raw(ptr.cast())
    }
}

impl<const MAX: i32> Default for ContinuationHistoryTable<MAX> {
    fn default() -> Self {
        Self {
            history: box_array(),
        }
    }
}

impl<const MAX: i32> ContinuationHistoryTable<MAX> {
    /// Get an index for the given move.
    fn index(m: Move, prev_piece: Piece, prev_tgt: Square) -> (usize, usize, usize, usize) {
        (
            prev_piece as usize,
            prev_tgt as usize,
            m.get_src() as usize,
            m.get_tgt() as usize,
        )
    }

    /// Add a history bonus value to the given move.
    fn add_bonus(&mut self, bonus: i16, m: Move, prev_piece: Piece, prev_tgt: Square) {
        let index = Self::index(m, prev_piece, prev_tgt);
        let old = &mut self.history[index.0][index.1][index.2][index.3];

        *old = taper_bonus::<MAX>(bonus, *old);
    }

    /// Update the history table after a beta cutoff.
    /// Gives a positive bonus to the fail-high move and a negative bonus to all other moves tried.
    pub fn update(&mut self, bonus: i16, best: Move, p: Piece, tgt: Square, searched: &Vec<Move>) {
        for m in searched {
            self.add_bonus(-bonus, *m, p, tgt);
        }
        self.add_bonus(bonus, best, p, tgt);
    }

    /// Get the double history score for a given move
    pub fn get_score(&self, m: Move, prev_piece: Piece, prev_tgt: Square) -> i32 {
        let index = Self::index(m, prev_piece, prev_tgt);

        self.history[index.0][index.1][index.2][index.3] as i32
    }
}

/// History tables used for captures.
///     Indexing: [capturing piece][tgt][captured piece]
#[derive(Clone, Debug)]
pub struct CaptureHistoryTable<const MAX: i32> {
    history: CaptureHistory,
}

impl<const MAX: i32> Default for CaptureHistoryTable<MAX> {
    fn default() -> Self {
        Self {
            history: [[[0; Piece::COUNT - 1]; Square::COUNT]; Piece::TOTAL],
        }
    }
}

impl<const MAX: i32> CaptureHistoryTable<MAX> {
    /// Get an index for the given move.
    fn index(m: Move, board: &Board) -> (usize, usize, usize) {
        (
            board.piece_at(m.get_src()) as usize,
            m.get_tgt() as usize,
            board.get_capture(m).index(),
        )
    }

    /// Add a history bonus value to the given move.
    fn add_bonus(&mut self, bonus: i16, m: Move, board: &Board) {
        let index = Self::index(m, board);
        let old = &mut self.history[index.0][index.1][index.2];

        *old = taper_bonus::<MAX>(bonus, *old);
    }

    /// Update the history table after a beta cutoff.
    /// Gives a positive bonus to the fail-high move and a negative bonus to all other moves tried.
    /// Can be called with a non-capture move to only give negative bonuses.
    pub fn update(&mut self, bonus: i16, best: Move, board: &Board, searched: &Vec<Move>) {
        for m in searched {
            self.add_bonus(-bonus, *m, board);
        }

        if best.get_type().is_capture() {
            self.add_bonus(bonus, best, board);
        }
    }

    /// Get the double history score for a given move
    pub fn get_score(&self, m: Move, board: &Board) -> i32 {
        let index = Self::index(m, board);

        self.history[index.0][index.1][index.2] as i32
    }
}
