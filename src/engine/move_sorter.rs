use crate::chess::{board::*, move_list::*, moves::*, piece::*};
use crate::engine::search_info::*;


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
#[derive(Clone, Debug, Default)]
pub struct MoveSorter {}

pub const TT_SCORE: i32 = 400;

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
    fn score_move(info: &mut SearchInfo, m: Move, board: &Board) -> i32 {
        let mt = m.get_type();

        if mt.is_capture() {
            Self::score_capture(m, board)
        } else if mt.is_promotion() {
            PROMOTION_SCORES[mt.get_promotion(board.side) as usize]
        } else if m == info.killer_moves[info.ply][0] {
            FIRST_KILLER
        } else if m == info.killer_moves[info.ply][1] {
            SECOND_KILLER
        } else if mt == MoveType::Castle {
            CASTLE_SCORE
        } else {
            Self::score_history(info, m, board.side)
        }
    }

    /// Score individual captures
    fn score_capture(m: Move, board: &Board) -> i32 {
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

        if board.see(m, 0) {
            score += GOOD_CAPTURE
        }

        score
    }

    /// Score quiet moves according to Standard/Counter Move/Follow Up heuristics
    fn score_history(info: &mut SearchInfo, m: Move, side: Color) -> i32 {
        let score = info.history.get_score(m, side)
            + info.counter_moves.get_score(m)
            + info.followup_moves.get_score(m);

        HISTORY_OFFSET + score
    }

    fn score<const QUIETS: bool>(info: &mut SearchInfo, m: Move, board: &Board) -> i32 {
        if QUIETS {
            Self::score_move(info, m, board)
        } else {
            Self::score_capture(m, board)
        }
    }

    /// Sort all moves in the movelist
    /// If QUIETS is false, it assumes all moves are captures and scores accordingly.
    pub fn score_moves<const QUIETS: bool>(
        info: &mut SearchInfo,
        board: &Board,
        move_list: &mut MoveList,
    ) {
        match info.tt_move {
            Some(tt_move) => {
                for i in 0..move_list.len() {
                    let m = move_list.moves[i];

                    move_list.scores[i] = if m == tt_move {
                        TT_SCORE
                    } else {
                        Self::score::<QUIETS>(info, m, board)
                    }
                }
            }

            None => {
                for i in 0..move_list.len() {
                    move_list.scores[i] = Self::score::<QUIETS>(info, move_list.moves[i], board)
                }
            }
        };
    }
}
