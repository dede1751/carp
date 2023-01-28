/// Implement move ordering through a MoveList and a Sorter for it
use crate::{moves::*, piece::*, square::*};

/// Taken from Pleco, adapted to 32b moves.
/// Assuming we have 8 cache lines, which should be 512B, we use up enough to fill a single
/// cache line with the array and a pointer of our current architecture.
#[cfg(target_pointer_width = "128")]
pub const MAX_MOVES: usize = 124;
#[cfg(target_pointer_width = "64")]
pub const MAX_MOVES: usize = 126;
#[cfg(target_pointer_width = "32")]
pub const MAX_MOVES: usize = 127;

/// MoveList with MoveScores, obtainable through a MoveSorter
pub struct MoveList {
    pub moves: [Move; MAX_MOVES],
    pub scores: [i16; MAX_MOVES],
    index: usize,
    len: usize,
}

/// Traverse movelist while sorting it by move score
/// This is taken from weiawaga as it's a pretty elegant way to maintain cache line constraints
/// while also keeping the move scores sorted correctly.
/// The sorting is O(n^2) but given the small size of the move list, the tradeoff is worth the
/// cache friendliness and avoids unnecessary sorting when reaching cutoffs
impl Iterator for MoveList {
    type Item = (Move, i16);

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.len {
            return None;
        }

        // find max score
        let mut best_score = 0;
        let mut best_index = self.index;
        for i in self.index..self.len {
            if self.scores[i] > best_score {
                best_score = self.scores[i];
                best_index = i;
            }
        }

        // swap max score to front
        self.moves.swap(self.index, best_index);
        self.scores.swap(self.index, best_index);
        self.index += 1;

        Some((self.moves[self.index - 1], self.scores[self.index - 1]))
    }
}

impl MoveList {
    pub fn new() -> MoveList {
        MoveList {
            moves: [NULL_MOVE; MAX_MOVES],
            scores: [0; MAX_MOVES],
            len: 0,
            index: 0,
        }
    }

    /// Returns move list length
    pub fn len(&self) -> usize {
        self.len
    }

    /// Push move into movelist
    fn push(&mut self, m: Move) {
        self.moves[self.len] = m;
        self.len += 1;
    }

    /// Initialize and add a capture move to the move list
    #[rustfmt::skip]
    pub fn add_capture(&mut self, src: Square, tgt: Square, piece: Piece, capture: Piece) {
        self.push(Move::encode(src, tgt, piece, capture, Piece::WP, 1, 0, 0, 0));
    }

    /// Initialize and add an enpassant capture to the move list
    #[rustfmt::skip]
    pub fn add_enpassant(&mut self, src: Square, tgt: Square, side: Color) {
        self.push(Move::encode(src, tgt, side.pawn(), (!side).pawn(), Piece::WP, 1, 0, 1, 0));
    }

    /// Adds pawn capture to move list, or all the possible promotions if on promotion rank
    #[rustfmt::skip]
    pub fn add_pawn_capture(&mut self, src: Square, tgt: Square, side: Color, capture: Piece) {
        if src.rank() == PROMOTION_RANKS[side as usize] {
            for promotion in PROMOTIONS[side as usize] {
                self.push(Move::encode(src, tgt, side.pawn(), capture, promotion, 1, 0, 0, 0));
            }
        } else {
            self.push(Move::encode(src, tgt, side.pawn(), capture, Piece::WP, 1, 0, 0, 0));
        }
    }

    /// Adds pawn quiet move to move list, or all the possible promotions if on promotion rank
    #[rustfmt::skip]
    pub fn add_pawn_quiet(&mut self, src: Square, tgt: Square, side: Color, double: u32) {
        if src.rank() == PROMOTION_RANKS[side as usize] {
            for promotion in PROMOTIONS[side as usize] {
                self.push(Move::encode(src, tgt, side.pawn(), Piece::WP, promotion, 0, 0, 0, 0));
            }
        } else {
            self.push(Move::encode(src, tgt, side.pawn(), Piece::WP, Piece::WP, 0, double, 0, 0));
        }
    }

    /// Initialize and add a quiet move to the move list
    #[rustfmt::skip]
    pub fn add_quiet(&mut self, src: Square, tgt: Square, piece: Piece, castle: u32) {
        self.push(Move::encode(src, tgt, piece, Piece::WP, Piece::WP, 0, 0, 0, castle));
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_movelist() {
        let mut l: MoveList = MoveList::new();

        l.add_pawn_capture(Square::E2, Square::D3, Color::White, Piece::BP);
        l.add_pawn_quiet(Square::E2, Square::E4, Color::White, 1);

        assert_eq!(l.len, 2);
    }
}
