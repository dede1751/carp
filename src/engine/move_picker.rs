use crate::chess::{board::*, move_list::*, moves::*, piece::*};
use crate::engine::{search_info::*, search_params::*};

/// Stages of the move picker.
/// Tacticals include both captures and promotions.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum Stage {
    TTMove,
    ScoreTacticals,
    GoodTacticals,
    Killer1,
    Killer2,
    ScoreQuiets,
    Quiets,
    BadTacticals,
    Done,
}

/// MovePicker is an iterator over the move list, yielding moves in order of heuristic value.
/// Moves are scored as lazily as possible since the move loop usually terminates very quickly.
///
/// In the end, the move list ends up structured as follows:
///
///                                                   v quiet_index
/// +--------------------------------------------------------------------------+
/// | TT MOVE | GOOD TACTICALS |  KILLER 1 | KILLER 2 | QUIETS | BAD TACTICALS |
/// +--------------------------------------------------------------------------+
///                            ^ good_tactical_index           ^ bad_tactical_index
///
pub struct MovePicker<const QUIETS: bool> {
    pub move_list: MoveList,
    scores: [i32; MAX_MOVES],
    index: usize,               // Index used for movelist traversal
    good_tactical_index: usize, // Index of the first non-tactical move
    bad_tactical_index: usize,  // Index of the first bad tactical move
    quiet_index: usize,         // Index of the first non-killer quiet move
    pub stage: Stage,           // Stage is not totally accurate externally, use move scores
    tt_move: Option<Move>,
    pub skip_quiets: bool,
    see_threshold: Eval,
}

/// Implement iterator functions
impl<const QUIETS: bool> MovePicker<QUIETS> {
    /// Initialize a new MovePicker for the given move list.
    pub fn new(
        move_list: MoveList,
        tt_move: Option<Move>,
        see_threshold: i32,
    ) -> MovePicker<QUIETS> {
        let bad_tactical_index = move_list.len();
        let stage = if move_list.is_empty() {
            Stage::Done
        } else {
            match tt_move {
                Some(m) if !QUIETS && m.get_type().is_quiet() => Stage::ScoreTacticals,
                None => Stage::ScoreTacticals,
                _ => Stage::TTMove,
            }
        };

        MovePicker {
            move_list,
            scores: [0; MAX_MOVES],
            index: 0,
            good_tactical_index: 0,
            bad_tactical_index,
            quiet_index: 0,
            stage,
            tt_move,
            skip_quiets: false,
            see_threshold,
        }
    }

    /// Fetch the next best move from the move list along with a move score.
    /// Note that most of the logic here is "fall through" where a stage may quietly pass without
    /// yielding a move (e.g. all scoring stages)
    pub fn next(&mut self, board: &Board, info: &SearchInfo) -> Option<(Move, i32)> {
        if self.stage == Stage::Done {
            return None;
        }

        // Lazily look for the TT move
        if self.stage == Stage::TTMove {
            self.stage = Stage::ScoreTacticals;

            let tt_move = self.tt_move.unwrap(); // We know it's Some
            if let Some(m) = self.find_pred(self.index, self.move_list.len(), |m| m == tt_move) {
                self.index += 1;
                return Some((m, TT_SCORE));
            }
        }

        // Assign a score to all captures/queen promotions and move them to the front.
        if self.stage == Stage::ScoreTacticals {
            self.stage = Stage::GoodTacticals;
            self.score_tacticals(board);
        }

        // Yield all captures/queen promotions with a positive SEE
        if self.stage == Stage::GoodTacticals {
            if let Some((m, s)) = self.partial_sort(self.good_tactical_index) {
                return Some((m, s));
            }

            // In QSearch we implicitly skip all captures with negative SEE and underpromotions.
            if QUIETS {
                self.stage = Stage::Killer1;
            } else {
                self.stage = Stage::Done;
                return None;
            }
        }

        // Lazily look for the first killer move (if we are not skipping quiets)
        if self.stage == Stage::Killer1 {
            self.stage = Stage::Killer2;

            let k1 = info.killer_moves[info.ply][0];
            if !self.skip_quiets && k1 != NULL_MOVE && self.tt_move != Some(k1) {
                let killer = self.find_pred(self.quiet_index, self.bad_tactical_index, |m| m == k1);

                if let Some(m) = killer {
                    self.quiet_index += 1;
                    return Some((m, KILLER1));
                }
            }
        }

        // Lazily look for the second killer move (if we are not skipping quiets)
        if self.stage == Stage::Killer2 {
            // If we are skipping quiets, we can just move to bad tacticals
            if !self.skip_quiets {
                self.stage = Stage::ScoreQuiets;
            } else {
                self.stage = Stage::BadTacticals;
                self.index = self.bad_tactical_index;
            }

            let k2 = info.killer_moves[info.ply][1];
            if !self.skip_quiets && k2 != NULL_MOVE && self.tt_move != Some(k2) {
                let killer = self.find_pred(self.quiet_index, self.bad_tactical_index, |m| m == k2);

                if let Some(m) = killer {
                    self.quiet_index += 1;
                    return Some((m, KILLER2));
                }
            }
        }

        // Assign a history score to all quiet moves
        if self.stage == Stage::ScoreQuiets {
            self.stage = Stage::Quiets;
            self.score_quiets(board.side, info);
        }

        // Yield all quiet moves/underpromotions
        if self.stage == Stage::Quiets {
            if !self.skip_quiets {
                if let Some((m, s)) = self.partial_sort(self.bad_tactical_index) {
                    return Some((m, s));
                }
            }

            self.stage = Stage::BadTacticals;
            self.index = self.bad_tactical_index;
        }

        // Yield all tactical moves with a negative SEE (and underpromotions if not skipping quiets)
        if self.stage == Stage::BadTacticals {
            if let Some((m, s)) = self.partial_sort(self.move_list.len()) {
                if !(self.skip_quiets && s == BAD_TACTICAL) {
                    return Some((m, s));
                }
            }

            self.stage = Stage::Done;
        }

        None
    }

    /// Yield the first move satisfying the given predicate within the range [start, end)
    /// Searches from the given starting index and swaps the move back to it.
    fn find_pred(&mut self, start: usize, end: usize, pred: impl Fn(Move) -> bool) -> Option<Move> {
        if start >= end {
            return None;
        }

        // Look for a move satisfying the predicate
        for i in start..end {
            let m = self.move_list.moves[i];

            if pred(m) {
                self.move_list.moves.swap(i, start);
                return Some(m);
            }
        }

        None
    }

    /// Perform a single iteration of Partial Insertion Sort.
    /// This is done over the range [self.index, end], with end excluded.
    ///
    /// O(n^2) sorting but takes advantage of early cutoffs which shouldn't require a full list
    /// sort. On top of that, we are exploiting the cache friendliness of linear memory access,
    /// and are usually limited to pretty short ranges.
    fn partial_sort(&mut self, end: usize) -> Option<(Move, i32)> {
        if self.index == end {
            return None;
        }

        let mut best_score = self.scores[self.index];
        let mut best_index = self.index;
        for i in (self.index + 1)..end {
            if self.scores[i] > best_score {
                best_score = self.scores[i];
                best_index = i;
            }
        }

        self.move_list.moves.swap(self.index, best_index);
        self.scores.swap(self.index, best_index);
        self.index += 1;

        Some((self.move_list.moves[self.index - 1], best_score))
    }
}

/// Special move scoring. We make sure these scores are much higher than any history score
pub const TT_SCORE: i32 = i32::MAX;
pub const KILLER1: i32 = GOOD_TACTICAL + 2;
pub const KILLER2: i32 = GOOD_TACTICAL + 1;

/// Tactical scoring: [10 - 70] + [1.000.000-2.000.000]
pub const GOOD_TACTICAL: i32 = 2_000_000;
pub const BAD_TACTICAL: i32 = 1_000_000;

const PROMO_SCORE: i32 = 70; // Queen promotions have best MVV-LVA value, but still less than good tacticals
const EP_SCORE: i32 = 15; // EP equal to pxp

pub const HISTORY_MAX: i32 = 49152; // Quiet moves have scores between +- 49152

// Attacker is the row, victim is the column
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

/// Score a single tactical move. These moves are either captures or queen promotions.
fn score_tactical(m: Move, see_threshold: Eval, board: &Board) -> i32 {
    // Underpromotions get the worst score
    if m.get_type().is_underpromotion() {
        return BAD_TACTICAL;
    }

    // Enpassant/QueenCapPromo are always good
    let mut score = match m.get_type() {
        MoveType::EnPassant => return GOOD_TACTICAL + EP_SCORE,
        MoveType::QueenCapPromo => return GOOD_TACTICAL + PROMO_SCORE,
        MoveType::QueenPromotion => PROMO_SCORE,
        _ => {
            let attacker = board.piece_at(m.get_src()) as usize;
            let victim = board.piece_at(m.get_tgt()) as usize;

            MVV_LVA[attacker][victim]
        }
    };

    // Give a bonus to moves with positive SEE
    if board.see(m, see_threshold) {
        score += GOOD_TACTICAL;
    } else {
        score += BAD_TACTICAL;
    }

    score
}

/// Implement ordering functions
impl<const QUIETS: bool> MovePicker<QUIETS> {
    /// Assign a score to each tactical move.
    /// Good tacticals are moved to the front of the list, bad tacticals to the back.
    fn score_tacticals(&mut self, board: &Board) {
        let mut i = self.index;
        self.good_tactical_index = self.index;

        while i < self.bad_tactical_index {
            let m = self.move_list.moves[i];

            if !QUIETS || !m.get_type().is_quiet() {
                let score = score_tactical(m, self.see_threshold, board);

                if score >= GOOD_TACTICAL {
                    self.move_list.moves.swap(i, self.good_tactical_index);
                    self.scores[self.good_tactical_index] = score;
                    self.good_tactical_index += 1;
                    i += 1;
                } else {
                    self.bad_tactical_index -= 1;
                    self.move_list.moves.swap(i, self.bad_tactical_index);
                    self.scores[self.bad_tactical_index] = score;
                }
            } else {
                i += 1;
            }
        }

        self.quiet_index = self.good_tactical_index; // quiets start after killers
    }

    /// Assign a score to each quiet move.
    /// Given the minimum score to underpromotions and a history score to the rest.
    fn score_quiets(&mut self, side: Color, info: &SearchInfo) {
        self.index = self.quiet_index; // skip over killers

        for i in self.index..self.bad_tactical_index {
            let m = self.move_list.moves[i];

            self.scores[i] = info.score_history(m, side);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chess::{square::*, tables::*};
    use crate::engine::{clock::*, tt::*};
    use std::sync;

    #[test]
    fn test_quiet_picker() {
        init_all_tables();

        let b: Board = "2r1k3/1P6/8/8/5b2/6P1/P7/2Q3K1 w - - 0 1".parse().unwrap();
        let move_list = b.gen_moves::<QUIETS>();
        let move_count = move_list.len();

        let tt_move = Move::new(Square::A2, Square::A4, MoveType::DoublePush);
        let good_cap = Move::new(Square::B7, Square::C8, MoveType::QueenCapPromo);
        let bad_promo = Move::new(Square::B7, Square::B8, MoveType::QueenPromotion);
        let k1 = Move::new(Square::A2, Square::A3, MoveType::Quiet);
        let k2 = Move::new(Square::A2, Square::A8, MoveType::Quiet); // impossible move
        let good_quiet = Move::new(Square::C1, Square::F1, MoveType::Quiet);
        let bad_quiet = Move::new(Square::G1, Square::H2, MoveType::Quiet);

        let mut picker = MovePicker::<QUIETS>::new(move_list, Some(tt_move), 0);

        let tt = TT::default();
        let clock = Clock::new(
            TimeControl::Infinite,
            sync::Arc::new(sync::atomic::AtomicBool::new(false)),
            true,
        );

        let mut info = SearchInfo::new(&tt, clock);
        info.update_tables(good_quiet, 10, Color::White, vec![bad_quiet]);
        info.killer_moves[info.ply][0] = k1;
        info.killer_moves[info.ply][1] = k2; // impossible move

        println!("{b}");

        let mut moves = Vec::new();
        while let Some(m) = picker.next(&b, &info) {
            println!("{:?} -- Move: {} Score: {}", picker.stage, m.0, m.1);
            moves.push(m);
        }

        assert_eq!(moves.len(), move_count);
        assert_eq!(moves[0].0, tt_move);
        assert_eq!(moves[1].0, good_cap);
        assert_eq!(moves[5].0, k1);
        assert_eq!(moves[6].0, good_quiet);
        assert_eq!(moves[26].0, bad_quiet);
        assert_eq!(moves[27].0, bad_promo);
        assert_eq!(moves[28].1, BAD_TACTICAL);
    }

    #[test]
    fn test_capture_picker() {
        init_all_tables();

        let b: Board = "2r1k3/1P6/8/8/5b2/6P1/P7/2Q3K1 w - - 0 1".parse().unwrap();
        let move_list = b.gen_moves::<CAPTURES>();
        let move_count = move_list.len();

        let tt_move = Move::new(Square::A2, Square::A4, MoveType::DoublePush);
        let good_cap = Move::new(Square::B7, Square::C8, MoveType::QueenCapPromo);

        let mut picker = MovePicker::<CAPTURES>::new(move_list, Some(tt_move), 0);

        let tt = TT::default();
        let clock = Clock::new(
            TimeControl::Infinite,
            sync::Arc::new(sync::atomic::AtomicBool::new(false)),
            true,
        );
        let info = SearchInfo::new(&tt, clock);

        println!("{b}");

        let mut moves = Vec::new();
        while let Some(m) = picker.next(&b, &info) {
            println!("{:?} -- Move: {} Score: {}", picker.stage, m.0, m.1);
            moves.push(m);
            assert!(!m.0.get_type().is_quiet());
        }

        assert_ne!(moves.len(), move_count);
        assert_ne!(moves[0].0, tt_move);
        assert_eq!(moves[0].0, good_cap);
    }
}
