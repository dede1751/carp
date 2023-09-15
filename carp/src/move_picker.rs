/// The Move Picker is responsible for choosing which move to search next at a node.
use crate::{search_params::*, thread::*};
use chess::{
    board::Board,
    move_list::MoveList,
    moves::{Move, MoveType},
    piece::Piece,
};

/// Stages of the move picker.
/// Tacticals include both captures and promotions.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum Stage {
    TTMove,
    ScoreTacticals,
    GoodTacticals,
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
///                             v quiet_index
/// +----------------------------------------------------+
/// | TT MOVE | GOOD TACTICALS  | QUIETS | BAD TACTICALS |
/// +----------------------------------------------------+
///                                      ^ bad_tactical_index
#[derive(Clone, Debug)]
pub struct MovePicker<const QUIETS: bool> {
    move_list: MoveList,
    scores: [i32; MoveList::SIZE],
    index: usize,              // Index used for movelist traversal
    quiet_index: usize,        // Index of the first non-killer quiet move
    bad_tactical_index: usize, // Index of the first bad tactical move
    pub stage: Stage,              // Stage is not totally accurate externally, use move scores
    tt_move: Option<Move>,
    pub skip_quiets: bool,
    see_threshold: Eval,
}

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
        } else if tt_move.is_none(){
            Stage::ScoreTacticals
        } else {
            Stage::TTMove
        };

        MovePicker {
            move_list,
            scores: [0; MoveList::SIZE],
            index: 0,
            quiet_index: 0,
            bad_tactical_index,
            stage,
            tt_move,
            skip_quiets: false,
            see_threshold,
        }
    }

    /// Fetch the next best move from the move list along with a move score.
    /// Note that most of the logic here is "fall through" where a stage may quietly pass without
    /// yielding a move (e.g. all scoring stages)
    pub fn next(&mut self, board: &Board, thread: &Thread, do_evasions: bool) -> Option<(Move, i32)> {
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
            self.score_tacticals(board, thread);
        }

        // Yield all captures/queen promotions with a positive SEE
        if self.stage == Stage::GoodTacticals {
            if let Some((m, s)) = self.partial_sort(self.quiet_index) {
                return Some((m, s));
            }

            // In QSearch we implicitly skip all captures with negative SEE and underpromotions,
            // unless we are in check and doing check evasions.
            if QUIETS || do_evasions {
                self.stage = Stage::ScoreQuiets;
            } else {
                self.stage = Stage::Done;
                return None;
            }
        }

        // Assign a history score to all quiet moves
        if self.stage == Stage::ScoreQuiets {
            self.stage = Stage::Quiets;

            thread.assign_history_scores(
                board.side,
                &self.move_list.moves[self.index..self.bad_tactical_index],
                &mut self.scores[self.index..self.bad_tactical_index],
            );
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

// TT Score must be greater than all other move values.
// Tactical scoring: [1_000_000-2_000_000] + [0-2400-4800-9600] +- (0:16_384) + [0-26384]
pub const TT_SCORE: i32 = i32::MAX;
pub const GOOD_TACTICAL: i32 = 2_000_000;
const BAD_TACTICAL: i32 = 1_000_000;
const PROMO_SCORE: i32 = CAP_HIST_MAX + MVV[4] + 1; // Queen promotions have best MVV + CapHist value
const MVV: [i32; Piece::COUNT] = [0, 2400, 2400, 4800, 9600, 0];

/// Score a single tactical move. These moves are either captures or queen promotions.
fn score_tactical(m: Move, see_threshold: Eval, board: &Board, thread: &Thread) -> i32 {
    // Enpassant/QueenCapPromo are always good
    let score = match m.get_type() {
        MoveType::QueenCapPromo => return GOOD_TACTICAL + PROMO_SCORE,
        MoveType::QueenPromotion => PROMO_SCORE,
        t if t.is_underpromotion() => return BAD_TACTICAL,
        _ => MVV[board.get_capture(m).index()] + thread.score_cap_hist(m, board),
    };

    // Give a bonus to moves with positive SEE
    if board.see(m, see_threshold) {
        score + GOOD_TACTICAL
    } else {
        score + BAD_TACTICAL
    }
}

/// Implement ordering functions
impl<const QUIETS: bool> MovePicker<QUIETS> {
    /// Assign a score to each tactical move.
    /// Good tacticals are moved to the front of the list, bad tacticals to the back.
    fn score_tacticals(&mut self, board: &Board, thread: &Thread) {
        let mut i = self.index;
        self.quiet_index = self.index;

        while i < self.bad_tactical_index {
            let m = self.move_list.moves[i];

            if !m.get_type().is_quiet() {
                let score = score_tactical(m, self.see_threshold, board, thread);

                if score >= GOOD_TACTICAL {
                    self.move_list.moves.swap(i, self.quiet_index);
                    self.scores[self.quiet_index] = score;
                    self.quiet_index += 1;
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chess::{
        board::{QUIETS, TACTICALS},
        square::Square,
    };

    #[test]
    fn test_quiet_picker() {
        let b: Board = "2r1k3/1P6/8/8/5b2/6P1/P7/2Q3K1 w - - 0 1".parse().unwrap();
        let move_list = b.gen_moves::<QUIETS>();
        let move_count = move_list.len();

        let tt_move = Move::new(Square::A2, Square::A4, MoveType::DoublePush);
        let good_cap = Move::new(Square::B7, Square::C8, MoveType::QueenCapPromo);
        let good_quiet = Move::new(Square::C1, Square::F1, MoveType::Quiet);
        let bad_quiet = Move::new(Square::G1, Square::H2, MoveType::Quiet);
        let bad_promo = Move::new(Square::B7, Square::B8, MoveType::QueenPromotion);

        let mut picker = MovePicker::<QUIETS>::new(move_list, Some(tt_move), 0);
        let mut t = Thread::fixed_depth(0);
        t.update_tables(good_quiet, 10, &Board::default(), vec![bad_quiet], vec![]);

        println!("{b}");

        let mut moves = Vec::new();
        while let Some(m) = picker.next(&b, &t, false) {
            println!(
                "{:?} -- Move {}: {} Score: {}",
                picker.stage,
                moves.len(),
                m.0,
                m.1
            );
            moves.push(m);
        }

        assert_eq!(moves.len(), move_count);
        assert_eq!(moves[0].0, tt_move);
        assert_eq!(moves[1].0, good_cap);
        assert_eq!(moves[5].0, good_quiet);
        assert_eq!(moves[26].0, bad_quiet);
        assert_eq!(moves[27].0, bad_promo);
        assert_eq!(moves[28].1, BAD_TACTICAL);
    }

    #[test]
    fn test_capture_picker() {
        let b: Board = "2r1k3/1P6/8/8/5b2/6P1/P7/2Q3K1 w - - 0 1".parse().unwrap();
        let move_list = b.gen_moves::<TACTICALS>();
        let move_count = move_list.len();

        let tt_move = Move::new(Square::A2, Square::A4, MoveType::DoublePush);
        let good_cap = Move::new(Square::B7, Square::C8, MoveType::QueenCapPromo);

        let mut picker = MovePicker::<TACTICALS>::new(move_list, Some(tt_move), 0);
        let t = Thread::fixed_depth(0);

        println!("{b}");

        let mut moves = Vec::new();
        while let Some(m) = picker.next(&b, &t, false) {
            println!("{:?} -- Move: {} Score: {}", picker.stage, m.0, m.1);
            moves.push(m);
            assert!(!m.0.get_type().is_quiet());
        }

        assert_ne!(moves.len(), move_count);
        assert_ne!(moves[0].0, tt_move);
        assert_eq!(moves[0].0, good_cap);
    }
}
