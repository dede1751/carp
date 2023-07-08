use std::cmp::min;

use crate::chess::{board::*, move_list::*, moves::*, piece::*, square::*};
use crate::engine::search_params::*;

type CMHistory = [[[[i32; SQUARE_COUNT]; SQUARE_COUNT]; SQUARE_COUNT]; PIECE_COUNT];
type FUHistory = [[[[i32; SQUARE_COUNT]; SQUARE_COUNT]; SQUARE_COUNT]; PIECE_COUNT];

/// Move Scoring
/// 400         -> TT Move
/// 300         -> capture queen promotions
/// 200         -> quiet queen promotion
/// 110:165     -> good and equal captures according to MVV-LVA
/// 110         -> enpassant
/// 102:103     -> second and first killer
/// 101         -> castling move
///  10: 65     -> bad captures according to MVV-LVA
///   0:-98304  -> quiet moves according to history score
/// i32::MIN    -> underpromotions
#[derive(Clone, Debug)]
pub struct MoveSorter {
    killer_moves: [[Move; 2]; MAX_DEPTH], // [ply][num_killer]
    history_moves: [[[i32; SQUARE_COUNT]; SQUARE_COUNT]; 2], // [color][from][to]
    pub counter: Option<(Move, Piece)>,
    counter_moves: Box<CMHistory>, // [piece+col][to][from][to]
    pub followup: Option<(Move, Piece)>,
    followup_moves: Box<FUHistory>, // [piece+col][to][from][to]
    pub tt_move: Option<Move>,
    pub see_threshold: Eval,
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

impl Default for MoveSorter {
    fn default() -> Self {
        MoveSorter {
            killer_moves: [[NULL_MOVE; 2]; MAX_DEPTH],
            history_moves: [[[0; SQUARE_COUNT]; SQUARE_COUNT]; 2],
            counter: None,
            counter_moves: box_array::<CMHistory>(),
            followup: None,
            followup_moves: box_array::<FUHistory>(),
            tt_move: None,
            see_threshold: 0,
        }
    }
}

/// Sorter updates
impl MoveSorter {
    /// Taper history so that it's bounded to 32 * 512 = 16384 (and -16384)
    /// Discussed here:
    /// http://www.talkchess.com/forum3/viewtopic.php?f=7&t=76540
    fn taper_bonus(bonus: i32, old: i32) -> i32 {
        old + 16 * bonus - old * bonus / 512
    }

    fn add_bonus(&mut self, m: Move, side: usize, bonus: i32) {
        let src = m.get_src() as usize;
        let tgt = m.get_tgt() as usize;
        let old_hist = self.history_moves[side][src][tgt];

        self.history_moves[side][src][tgt] = Self::taper_bonus(bonus, old_hist);
    }

    fn update_history(&mut self, curr: Move, bonus: i32, side: usize, searched: &Vec<Move>) {
        for m in searched {
            self.add_bonus(*m, side, -bonus)
        }
        self.add_bonus(curr, side, bonus);
    }

    fn add_double_bonus<const CMH: bool>(&mut self, m: Move, p: usize, t: usize, bonus: i32) {
        let src = m.get_src() as usize;
        let tgt = m.get_tgt() as usize;

        if CMH {
            let old = self.counter_moves[p][t][src][tgt];
            self.counter_moves[p][t][src][tgt] = Self::taper_bonus(bonus, old);
        } else {
            let old = self.followup_moves[p][t][src][tgt];
            self.followup_moves[p][t][src][tgt] = Self::taper_bonus(bonus, old);
        }
    }

    fn update_double<const CMH: bool>(
        &mut self,
        curr: Move,
        prev: Move,
        piece: Piece,
        bonus: i32,
        searched: &Vec<Move>,
    ) {
        let prev_t = prev.get_tgt() as usize;

        for m in searched {
            self.add_double_bonus::<CMH>(*m, piece as usize, prev_t, -bonus)
        }
        self.add_double_bonus::<CMH>(curr, piece as usize, prev_t, bonus);
    }

    /// Update killer and history values for sorting quiet moves after a beta cutoff
    pub fn update(&mut self, m: Move, ply: usize, depth: usize, side: Color, searched: Vec<Move>) {
        let first_killer = self.killer_moves[ply][0];

        if first_killer != m {
            self.killer_moves[ply][1] = first_killer;
            self.killer_moves[ply][0] = m;
        }

        // leaves can introduce a lot of random noise to history scores, don't consider them
        if depth < HISTORY_LOWER_LIMIT {
            return;
        }

        // history bonus is Stockfish's "gravity"
        let bonus = min(depth * depth, 400) as i32;

        self.update_history(m, bonus, side as usize, &searched);

        // countermove and followup history
        const CMH: bool = true;
        const FUH: bool = false;
        if let Some((counter_move, piece)) = self.counter {
            self.update_double::<CMH>(m, counter_move, piece, bonus, &searched);

            if let Some((followup_move, piece)) = self.followup {
                self.update_double::<FUH>(m, followup_move, piece, bonus, &searched);
            }
        }
    }
}

const TT_SCORE: i32 = 400;

pub const GOOD_CAPTURE: i32 = 100;
const EP_SCORE: i32 = GOOD_CAPTURE + 10;
const FIRST_KILLER: i32 = GOOD_CAPTURE + 3;
const SECOND_KILLER: i32 = GOOD_CAPTURE + 2;
const CASTLE_SCORE: i32 = GOOD_CAPTURE + 1;

pub const HISTORY_OFFSET: i32 = -(3 * 16384);
const WORST: i32 = i32::MIN;

const PROMOTION_SCORES: [i32; PIECE_COUNT] = [
    0, 0, WORST, WORST, WORST, WORST, WORST, WORST, 200, 200, 0, 0,
];

#[rustfmt::skip]
const MVV_LVA: [[i32; PIECE_COUNT]; PIECE_COUNT] = [
//    WP  BP  WN  BN  WB  BB  WR  BR  WQ  BQ  WK  BK 
    [ 15, 15, 25, 25, 35, 35, 45, 45, 55, 55, 65, 65 ], // WP
    [ 15, 15, 25, 25, 35, 35, 45, 45, 55, 55, 65, 65 ], // BP
    [ 14, 14, 24, 24, 34, 34, 44, 44, 54, 54, 64, 64 ], // WN
    [ 14, 14, 24, 24, 34, 34, 44, 44, 54, 54, 64, 64 ], // BN
    [ 13, 13, 23, 23, 33, 33, 43, 43, 53, 53, 63, 63 ], // WB
    [ 13, 13, 23, 23, 33, 33, 43, 43, 53, 53, 63, 63 ], // BB
    [ 12, 12, 22, 22, 32, 32, 42, 42, 52, 52, 62, 62 ], // WR
    [ 12, 12, 22, 22, 32, 32, 42, 42, 52, 52, 62, 62 ], // BR
    [ 11, 11, 21, 21, 31, 31, 41, 41, 51, 51, 61, 61 ], // WQ
    [ 11, 11, 21, 21, 31, 31, 41, 41, 51, 51, 61, 61 ], // BQ
    [ 10, 10, 20, 20, 30, 30, 40, 40, 50, 50, 60, 60 ], // WK
    [ 10, 10, 20, 20, 30, 30, 40, 40, 50, 50, 60, 60 ], // BK
];

/// Move Scoring
impl MoveSorter {
    /// Score any type of move
    fn score_move(&self, m: Move, board: &Board, ply: usize) -> i32 {
        let mt = m.get_type();

        if mt.is_capture() {
            self.score_capture(m, board)
        } else if mt.is_promotion() {
            PROMOTION_SCORES[mt.get_promotion(board.side) as usize]
        } else if m == self.killer_moves[ply][0] {
            FIRST_KILLER
        } else if m == self.killer_moves[ply][1] {
            SECOND_KILLER
        } else if mt == MoveType::Castle {
            CASTLE_SCORE
        } else {
            self.score_history(m, board.side as usize)
        }
    }

    /// Score individual captures
    fn score_capture(&self, m: Move, board: &Board) -> i32 {
        // Capture promotions can never have negative SEE, enpassant is usually not bad
        let mt = m.get_type();
        if mt.is_promotion() {
            return PROMOTION_SCORES[mt.get_promotion(board.side) as usize] + GOOD_CAPTURE;
        } else if mt == MoveType::EnPassant {
            return EP_SCORE;
        }

        // Handle normal captures by MVV/LVA + SEE
        let attacker = board.piece_at(m.get_src()) as usize;
        let victim = board.piece_at(m.get_tgt()) as usize;
        let mut score = MVV_LVA[attacker][victim];

        if board.see(m, self.see_threshold) {
            score += GOOD_CAPTURE
        }

        score
    }

    /// Score quiet moves according to Standard/Counter Move/Follow Up heuristics
    fn score_history(&self, m: Move, side: usize) -> i32 {
        let src = m.get_src() as usize;
        let tgt = m.get_tgt() as usize;

        let mut score = self.history_moves[side][src][tgt];

        // counter move
        if let Some((counter_move, piece)) = self.counter {
            let prev_tgt = counter_move.get_tgt() as usize;

            score += self.counter_moves[piece as usize][prev_tgt][src][tgt];

            // followup move
            if let Some((followup_move, piece)) = self.followup {
                let prev_tgt = followup_move.get_tgt() as usize;

                score += self.followup_moves[piece as usize][prev_tgt][src][tgt];
            }
        }

        HISTORY_OFFSET + score
    }

    fn score<const QUIETS: bool>(&self, m: Move, board: &Board, ply: usize) -> i32 {
        if QUIETS {
            self.score_move(m, board, ply)
        } else {
            self.score_capture(m, board)
        }
    }

    /// Sort all moves in the movelist
    /// If QUIETS is false, it assumes all moves are captures and scores accordingly.
    pub fn score_moves<const QUIETS: bool>(
        &self,
        board: &Board,
        ply: usize,
        move_list: &mut MoveList,
    ) {
        match self.tt_move {
            Some(tt_move) => {
                for i in 0..move_list.len() {
                    let m = move_list.moves[i];

                    move_list.scores[i] = if m == tt_move {
                        TT_SCORE
                    } else {
                        self.score::<QUIETS>(m, board, ply)
                    }
                }
            }

            None => {
                for i in 0..move_list.len() {
                    move_list.scores[i] = self.score::<QUIETS>(move_list.moves[i], board, ply)
                }
            }
        };
    }
}
