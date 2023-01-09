/// Implement move ordering through a MoveList and a Sorter for it

use crate::{
    square::*,
    piece::*,
    moves::*,
    search::MAX_DEPTH,
};

/// MoveList, saves captures and quiets independently.
pub struct MoveList {
    pub moves: Vec<Move>,
}

impl IntoIterator for MoveList {
    type Item = Move;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    // iterate over all the moves
    fn into_iter(self) -> Self::IntoIter {
        self.moves.into_iter()
    }
}

impl MoveList {
    pub fn new() -> MoveList {
        MoveList {
            moves: Vec::with_capacity(50),
        }
    }

    /// Returns total length of the move list
    #[inline]
    pub fn len(&self) -> usize {
        self.moves.len()
    }

    /// Initialize and add a capture move to the capture vector
    #[inline]
    pub fn add_capture(&mut self, src: Square, tgt: Square, piece: Piece, capture: Piece) {
        self.moves.push(
            Move::encode(src, tgt, piece, capture, Piece::WP, 1, 0, 0, 0)
        );
    }

    /// Initialize and add an en-passant capture to the move list
    #[inline]
    pub fn add_enpassant(&mut self, src: Square, tgt: Square, side: Color) {
        self.moves.push(
            Move::encode(src, tgt, side.pawn(), (!side).pawn(), Piece::WP, 1, 0, 1, 0)
        );
    }

    /// Adds pawn capture to move list, or all the possible promotions if on promotion rank
    #[inline]
    pub fn add_pawn_capture(&mut self, src: Square, tgt: Square, side: Color, capture: Piece) {
        if src.rank() == PROMOTION_RANKS[side as usize] {
            for promotion in PROMOTIONS[side as usize] {
                self.moves.push(
                    Move::encode(src, tgt, side.pawn(), capture, promotion, 1, 0, 0, 0)
                );
            }
        } else {
            self.moves.push(
                Move::encode(src, tgt, side.pawn(), capture, Piece::WP, 1, 0, 0, 0)
            );
        }
    }

    /// Adds pawn quiet move to move list, or all the possible promotions if on promotion rank
    #[inline]
    pub fn add_pawn_quiet(&mut self, src: Square, tgt: Square, side: Color, double_push: u32) {
        if src.rank() == PROMOTION_RANKS[side as usize] {
            for promotion in PROMOTIONS[side as usize] {
                self.moves.push(
                    Move::encode(src, tgt, side.pawn(), Piece::WP, promotion, 0, 0, 0, 0)
                );
            }
        } else {    
            self.moves.push(
                Move::encode(src, tgt, side.pawn(), Piece::WP, Piece::WP, 0, double_push, 0, 0)
            );
        }
    }

    /// Initialize and add a quiet move to the quiet vector
    #[inline]
    pub fn add_quiet(&mut self, src: Square, tgt: Square, piece: Piece, castle: u32) {
        self.moves.push(
            Move::encode(src, tgt, piece, Piece::WP, Piece::WP, 0, 0, 0, castle)
        );
    }
}

/// Move Scoring - lower is better
/// 
/// * TT moves when found are scored best.
/// * Promotions receive an extra score to always put them at the top of the list. Knight promotions
///   are ordered after queen because they are the most likely underpromotion.
/// * Captures are evaluated with MVV-LVA values            MoveScore = [-100 -> -605]
/// * Quiet moves are evaluated in the following order:
///     - Killer moves are awarded the most points:
///         first and second killer from current ply get -5 and -4 points
///         first and second killer from 2 plies ago get -3 and -2 points
///     - Castling always comes after killers and before all other quiets
///     - Remaining quiets all have positive scores, obtained from the history score + 30000.
///       history moves are reset so as to not have them get positive scores.
type MoveScore = i32;
const TT_SCORE: MoveScore = -10000;
const PROMOTION_OFFSETS: [MoveScore; PIECE_COUNT] = [
    0, 0, -7000, -7000, -5000, -5000, -6000, -6000, -8000, -8000, 0, 0,
];
const MVV_LVA: [[MoveScore; PIECE_COUNT]; PIECE_COUNT] = [
//      WP    BP    WN    BN    WB    BB    WR    BR    WQ    BQ    WK    BK 
    [ -105, -105, -205, -205, -305, -305, -405, -405, -505, -505, -605, -605 ], // WP
    [ -105, -105, -205, -205, -305, -305, -405, -405, -505, -505, -605, -605 ], // BP
    [ -104, -104, -204, -204, -304, -304, -404, -404, -504, -504, -604, -604 ], // WN
    [ -104, -104, -204, -204, -304, -304, -404, -404, -504, -504, -604, -604 ], // BN
    [ -103, -103, -203, -203, -303, -303, -403, -403, -503, -503, -603, -603 ], // WB
    [ -103, -103, -203, -203, -303, -303, -403, -403, -503, -503, -603, -603 ], // BB
    [ -102, -102, -202, -202, -302, -302, -402, -402, -502, -502, -602, -602 ], // WR
    [ -102, -102, -202, -202, -302, -302, -402, -402, -502, -502, -602, -602 ], // BR
    [ -101, -101, -201, -201, -301, -301, -401, -401, -501, -501, -601, -601 ], // WQ
    [ -101, -101, -201, -201, -301, -301, -401, -401, -501, -501, -601, -601 ], // BQ
    [ -100, -100, -200, -200, -300, -300, -400, -400, -500, -500, -600, -600 ], // WK
    [ -100, -100, -200, -200, -300, -300, -400, -400, -500, -500, -600, -600 ], // BK
];
const FIRST_KILLER_OFFSET: MoveScore = -5;
const SECOND_KILLER_OFFSET: MoveScore = -4;
const CASTLE_SCORE: MoveScore = -1;

const MAX_KILLERS: usize = 2;

const HISTORY_OFFSET: MoveScore = 30000;


#[derive(Debug)]
pub struct MoveSorter {
    killer_moves: [[Move; MAX_KILLERS]; MAX_DEPTH as usize],
    history_moves: [[MoveScore; SQUARE_COUNT]; PIECE_COUNT],
    pub tt_move: Option<Move>,
}

impl MoveSorter {
    pub fn new() -> MoveSorter {
        MoveSorter {
            killer_moves : [[NULL_MOVE; MAX_KILLERS]; MAX_DEPTH as usize],
            history_moves: [[0; SQUARE_COUNT]; PIECE_COUNT],
            tt_move: None,
        }
    }

    /// Save quiet moves that caused a cutoff at given ply
    #[inline]
    pub fn add_killer(&mut self, m: Move, ply: u8) {
        let ply = ply as usize;
        let first_killer = self.killer_moves[ply][0];

        if first_killer != m {
            self.killer_moves[ply][1] = first_killer;
            self.killer_moves[ply][0] = m;
        }
    }

    /// Increase cutoff move's history score
    #[inline]
    pub fn add_history(&mut self, m: Move, depth: u8) {
        let p = m.get_piece() as usize;
        let sq = m.get_tgt() as usize;
        self.history_moves[p][sq] -= (depth * depth) as MoveScore;

        // reset history values when surpassing captures/promotions/killers
        if self.history_moves[p][sq] <= -HISTORY_OFFSET {
            for p in ALL_PIECES {
                for sq in ALL_SQUARES {
                    self.history_moves[p as usize][sq as usize] >>= 1;
                }
            }
        }
    }

    #[inline]
    fn score_capture(m: Move) -> MoveScore {
        MVV_LVA[m.get_piece() as usize][m.get_capture() as usize]
    }

    #[inline]
    fn score_killer(&self, m: Move, ply: u8) -> MoveScore {
        if m == self.killer_moves[ply as usize][0] {
            FIRST_KILLER_OFFSET
        } else if m == self.killer_moves[ply as usize][1] {
            SECOND_KILLER_OFFSET
        } else if ply >= 2 {
            self.score_killer(m, ply - 2) + 2 // older killer valued -3, -2
        } else {
            0
        }
    }

    fn score_move(&self, m: Move, ply: u8) -> MoveScore {
        if m.is_capture() {
            Self::score_capture(m) + PROMOTION_OFFSETS[m.get_promotion() as usize]
        } else { // quiets
            let mut score: MoveScore = 0;

            score += PROMOTION_OFFSETS[m.get_promotion() as usize];
            score += self.score_killer(m, ply);
            if m.is_castle() { score += CASTLE_SCORE; }

            if score == 0 { // neither castle nor killer
                let p = m.get_piece() as usize;
                let sq = m.get_tgt() as usize;
                score += self.history_moves[p][sq] + HISTORY_OFFSET;
            };

            score
        }
    }

    /// Sort only the captures in the movelist
    #[inline]
    pub fn sort_captures(&self, move_list: &mut MoveList) {
        move_list.moves.sort_by_key(|m|{
            Self::score_capture(*m)
        });
    }

    /// Sort all moves in the movelist
    #[inline]
    pub fn sort_moves(&self, move_list: &mut MoveList, ply: u8) {
        match self.tt_move {
            Some(tt_move) => {
                move_list.moves.sort_by_key(|m| {
                    if *m == tt_move {
                        TT_SCORE
                    } else {
                        self.score_move(*m, ply)
                    }
                });
            }
            None => move_list.moves.sort_by_key(|m| { self.score_move(*m, ply) })
        };
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        board::*,
        tables::Tables,
    };

    #[test]
    fn test_movelist() {
        let mut l: MoveList = MoveList::new();

        l.add_pawn_capture(Square::E2, Square::D3, Color::White, Piece::BP);
        l.add_pawn_quiet(Square::E2, Square::E4, Color::White, 1);

        let full_list: Vec<Move> = l.into_iter().collect();
        assert_eq!(full_list.len(), 2);
    }

    #[test]
    fn test_sorter() {
        let b: Board = Board::try_from(
            "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"
        ).unwrap();
        let t: Tables = Tables::default();
        let ms: MoveSorter = MoveSorter::new();

        let mut move_list = b.generate_moves(&t);
        let len = move_list.len();

        ms.sort_moves(&mut move_list, 0);

        assert_eq!(move_list.len(), len);
    }
}