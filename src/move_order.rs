use std::iter::Chain;
use std::vec::IntoIter;

use crate::{ Square, Piece, Move, PIECE_COUNT, NULL_MOVE, SQUARE_COUNT, ALL_PIECES, ALL_SQUARES };

/// # MoveList -- Separates captures and quiets and allows for move ordering
pub struct MoveList {
    pub captures: Vec<Move>,
    pub quiets  : Vec<Move>,
}

impl IntoIterator for MoveList {
    type Item = Move;
    type IntoIter = Chain<IntoIter<Self::Item>, IntoIter<Self::Item>>;

    // chains the two move lists
    fn into_iter(self) -> Self::IntoIter {
        self.captures.into_iter().chain(self.quiets.into_iter())
    }
}

impl MoveList {
    pub fn new() -> MoveList {
        MoveList {
            captures: Vec::with_capacity(16),
            quiets: Vec::with_capacity(16),
        }
    }

    /// Returns total length of the move list
    #[inline]
    pub fn len(&self) -> usize {
        self.captures.len() + self.quiets.len()
    }

    /// Initialize and add a capture move to the capture vector
    #[inline]
    pub fn add_capture(
        &mut self,
        src: Square,
        tgt: Square,
        piece: Piece,
        capture: Piece,
        promote: Piece,
        en_passant: u32
    ) {
        self.captures.push(
            Move::encode(src, tgt, piece, capture, promote, 1, 0, en_passant, 0)
        )
    }

    /// Initialize and add a quiet move to the quiet vector
    #[inline]
    pub fn add_quiet(
        &mut self,
        src: Square,
        tgt: Square,
        piece: Piece,
        promote: Piece,
        double_push: u32,
        castle: u32
    ) {
        self.quiets.push(
            Move::encode(src, tgt, piece, Piece::WP, promote, 0, double_push, 0, castle)
        )
    }
}

/// # Move Scoring - lower is better
/// 
/// * Promotions receive an extra score to always put them at the top of the list. Knight promotions
///   are ordered after queen because they are the most likely underpromotion.
///   will get 
/// * Captures are evaluated with MVV-LVA values            MoveScore = [-100 -> -605]
/// * Quiet moves are evaluated in the following order:
///     - Killer moves are awarded the most points:
///         first and second killer from current ply get -5 and -4 points
///         first and second killer from 2 plies ago get -3 and -2 points
///     - Castling is awarded -1 point
///     - Remaining quiets all have positive scores, obtained from the history score + 30000.
///       history moves are reset so as to not have them get positive scores.

type MoveScore = i32;
const MVV_LVA: [[MoveScore; PIECE_COUNT]; PIECE_COUNT] = [
//    WP  BP  WN  BN  WB  BB  WR  BR  WQ  BQ  WK  BK 
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
const PROMOTION_OFFSETS: [MoveScore; PIECE_COUNT] = [
    0, 0, -7000, -7000, -5000, -5000, -6000, -6000, -8000, -8000, 0, 0,
];
const CASTLE_SCORE: MoveScore = -1;

const MAX_DEPTH: usize = 125;
const MAX_KILLERS: usize = 2;
const FIRST_KILLER_OFFSET: MoveScore = -5;
const SECOND_KILLER_OFFSET: MoveScore = -4;

const HISTORY_OFFSET: MoveScore = 30000;


#[derive(Debug)]
pub struct MoveSorter {
    killer_moves: [[Move; MAX_KILLERS]; MAX_DEPTH],
    history_moves: [[MoveScore; SQUARE_COUNT]; PIECE_COUNT],
}

impl MoveSorter {
    pub fn new() -> MoveSorter {
        MoveSorter {
            killer_moves : [[NULL_MOVE; MAX_KILLERS]; MAX_DEPTH],
            history_moves: [[0; SQUARE_COUNT]; PIECE_COUNT],
        }
    }

    #[inline]
    pub fn add_killer(&mut self, m: Move, ply: usize) {
        let first_killer = self.killer_moves[ply][0];

        if first_killer != m {
            self.killer_moves[ply][1] = first_killer;
            self.killer_moves[ply][0] = m;
        }
    }

    #[inline]
    pub fn add_history(&mut self, m: Move, depth: usize) {
        let p = m.get_piece().index();
        let sq = m.get_tgt().index();
        self.history_moves[p][sq] -= (depth * depth) as MoveScore;

        // reset history values when surpassing captures/promotions/killers
        if self.history_moves[p][sq] <= -HISTORY_OFFSET {
            for p in ALL_PIECES {
                for sq in ALL_SQUARES {
                    self.history_moves[p.index()][sq.index()] >>= 1;
                }
            }
        }
    }

    #[inline]
    fn score_capture(m: &Move) -> MoveScore {
        MVV_LVA[m.get_piece().index()][m.get_capture().index()]
    }

    #[inline]
    fn score_killer(&self, m: &Move, ply: usize) -> MoveScore {
        if *m == self.killer_moves[ply][0] {
            FIRST_KILLER_OFFSET
        } else if *m == self.killer_moves[ply][1] {
            SECOND_KILLER_OFFSET
        } else if ply >= 2 {
            self.score_killer(m, ply - 2) + 2 // older killer valued -3, -2
        } else {
            0
        }
    }

    #[inline]
    fn score_move(&self, m: &Move, ply: usize) -> MoveScore {
        let mut score: MoveScore = 0;

        if m.is_capture() {
            score += Self::score_capture(m);
        } else { // quiets
            score += self.score_killer(m, ply);
            if m.is_castle() {
                score += CASTLE_SCORE;
            }

            if score == 0 { // neither castle nor killer
                let p = m.get_piece().index();
                let sq = m.get_tgt().index();
                score += self.history_moves[p][sq] + HISTORY_OFFSET;
            }
        };
        
        score + PROMOTION_OFFSETS[m.get_promotion().index()]
    }

    pub fn sort_captures(&self, mut move_list: MoveList) -> Vec<Move> {
        move_list.captures.sort_by_key(|m|{ Self::score_capture(m) });
        move_list.captures
    }

    pub fn sort_moves(&self, mut move_list: MoveList, ply: usize) -> Vec<Move> {
        let mut all_moves = move_list.captures;

        all_moves.append(&mut move_list.quiets);
        all_moves.sort_by_key(|m| { self.score_move(m, ply as usize) });

        all_moves
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_movelist() {
        let mut l: MoveList = MoveList::new();

        l.add_capture(Square::E2, Square::D3, Piece::WP, Piece::BP, Piece::WP, 0);
        l.add_quiet(Square::E2, Square::E4, Piece::WP, Piece::WP, 1, 0);

        let full_list: Vec<Move> = l.into_iter().collect();
        assert_eq!(full_list.len(), 2);
    }

}