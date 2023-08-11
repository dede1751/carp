/// History Tables are structures used for ordering quiet moves.
/// Whenever a quiet move fails high, it receives a history bonus.
/// Since it would be impossible to precisely index each move, history tables have different indexing
/// schemes depending on the sort of "history" they are trying to track.
use crate::chess::{moves::*, piece::*, square::*};

pub type History = [[[i16; SQUARE_COUNT]; SQUARE_COUNT]; 2];
pub type DoubleHistory = [[[[i16; SQUARE_COUNT]; SQUARE_COUNT]; SQUARE_COUNT]; PIECE_COUNT];

/// History bonus is Stockfish's "gravity"
pub fn history_bonus(depth: usize) -> i16 {
    400.min(depth * depth) as i16
}

/// Taper history so that it's bounded to +-(2048 * 8)
/// This keeps us within i16 bounds.
/// Discussed here:
/// http://www.talkchess.com/forum3/viewtopic.php?f=7&t=76540
const fn taper_bonus(bonus: i16, old: i16) -> i16 {
    let o = old as i32;
    let b = bonus as i32;

    // Use i32's to avoid overflows
    (o + 8 * b - (o * b.abs()) / 2048) as i16
}

/// Simple history tables are used for standard move histories.
///     Indexing: [side][src][tgt]
pub struct HistoryTable {
    history: History,
}

impl Default for HistoryTable {
    fn default() -> Self {
        HistoryTable {
            history: [[[0; SQUARE_COUNT]; SQUARE_COUNT]; 2],
        }
    }
}

impl HistoryTable {
    /// Add a history bonus value to the given move.
    fn add_bonus(&mut self, bonus: i16, m: Move, side: Color) {
        let src = m.get_src() as usize;
        let tgt = m.get_tgt() as usize;
        let old = &mut self.history[side as usize][src][tgt];

        *old = taper_bonus(bonus, *old);
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
        let src = m.get_src() as usize;
        let tgt = m.get_tgt() as usize;

        self.history[side as usize][src][tgt] as i32
    }
}

/// Double history tables are used for counter moves and followup moves.
/// The first two indices are taken from the history move, the last two from the current move.
///    - Counter Move: the previous move by the opponent
///    - Followup Move: our previous move
///
///     Indexing: [old_piece][old_tgt][new_src][new_tgt]
pub struct DoubleHistoryTable {
    history: Box<DoubleHistory>,
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

impl Default for DoubleHistoryTable {
    fn default() -> Self {
        DoubleHistoryTable {
            history: box_array::<DoubleHistory>(),
        }
    }
}

impl DoubleHistoryTable {
    /// Add a history bonus value to the given move.
    fn add_bonus(&mut self, bonus: i16, m: Move, p: usize, t: usize) {
        let src = m.get_src() as usize;
        let tgt = m.get_tgt() as usize;
        let old = &mut self.history[p][t][src][tgt];

        *old = taper_bonus(bonus, *old);
    }

    /// Update the history table after a beta cutoff.
    /// Gives a positive bonus to the fail-high move and a negative bonus to all other moves tried.
    pub fn update(&mut self, bonus: i16, best: Move, p: Piece, tgt: Square, searched: &Vec<Move>) {
        for m in searched {
            self.add_bonus(-bonus, *m, p as usize, tgt as usize);
        }
        self.add_bonus(bonus, best, p as usize, tgt as usize);
    }

    /// Get the double history score for a given move
    pub fn get_score(&self, m: Move, p: Piece, prev_tgt: Square) -> i32 {
        let src = m.get_src() as usize;
        let tgt = m.get_tgt() as usize;

        self.history[p as usize][prev_tgt as usize][src][tgt] as i32
    }
}
