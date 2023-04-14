use std::{cmp::min, str::FromStr};

use crate::bitboard::*;
use crate::board::*;
use crate::move_list::*;
use crate::move_sorter::*;
use crate::moves::*;
use crate::nnue::*;
use crate::piece::*;
use crate::search_params::*;
use crate::zobrist::*;

/// Position, represents a Board's evolution along the search tree.
/// Also incorporates move ordering and various game rules (50mr, draw detection etc)
#[derive(Clone, Debug)]
pub struct Position {
    pub board: Board,
    pub age: u8,
    pub ply: usize,
    ply_from_null: usize,
    history: Vec<(Board, Move, usize)>,
    sorter: MoveSorter,
    nnue_state: Box<NNUEState>,
}

/// Get position from uci position string
impl FromStr for Position {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();
        let mut board: Board = match tokens.next() {
            Some("startpos") => Board::default(),
            Some("fen") => {
                let fen = &tokens.clone().take(6).collect::<Vec<&str>>().join(" ")[..];

                for _ in 0..6 {
                    tokens.next();
                }

                fen.parse()?
            }
            _ => return Err("Invalid position"),
        };

        let mut ply_from_null = 0;
        let mut history = Vec::new();

        if let Some("moves") = tokens.next() {
            for move_str in tokens {
                let new = board.find_move(move_str);

                match new {
                    Some(m) => {
                        history.push((board, m, ply_from_null));

                        board = board.make_move(m);
                        ply_from_null += 1;
                    }
                    None => eprintln!("Move is not legal!"),
                };
            }
        };

        let res = Position {
            board,
            age: history.len() as u8 + 1,
            ply: 0,
            ply_from_null,
            history,
            sorter: MoveSorter::default(),
            nnue_state: NNUEState::from_board(&board),
        };

        Ok(res)
    }
}

/// Default position is startpos
impl Default for Position {
    fn default() -> Self {
        "startpos".parse().unwrap()
    }
}

impl Position {
    /// Returns the current board's hash
    pub fn hash(&self) -> ZHash {
        self.board.hash
    }

    /// Returns true if it's white to move
    pub fn white_to_move(&self) -> bool {
        self.board.side == Color::White
    }

    /// Generate a sorted list of captures
    pub fn generate_captures(&self) -> MoveList {
        let mut move_list = self.board.gen_moves::<CAPTURES>();

        self.sorter.score_captures(&self.board, &mut move_list);
        move_list
    }

    /// Generate a sorted list of moves
    pub fn generate_moves(&self) -> MoveList {
        let mut move_list = self.board.gen_moves::<QUIETS>();

        self.sorter.score_moves(self, &mut move_list);
        move_list
    }

    /// Makes the given move
    pub fn make_move(&mut self, m: Move) {
        let new = self.board.make_move_nnue(m, &mut self.nnue_state);

        self.sorter.followup_move = self.sorter.counter_move;
        self.sorter.counter_move = Some(m);

        self.history.push((self.board, m, self.ply_from_null));
        self.board = new;
        self.ply += 1;
        self.ply_from_null += 1;
    }

    /// Passes turn to opponent (this resets the ply_from_null clock)
    pub fn make_null(&mut self) {
        let new = self.board.make_null();
        self.nnue_state.push();

        self.sorter.followup_move = None;
        self.sorter.counter_move = None;

        self.history
            .push((self.board, NULL_MOVE, self.ply_from_null));
        self.board = new;
        self.ply += 1;
        self.ply_from_null = 0;
    }

    /// Pops the current board, going back to previous history entry
    /// Panics if the history vector is empty!
    pub fn undo_move(&mut self) {
        let (old_board, _, old_ply) = self.history.pop().unwrap();
        self.nnue_state.pop();

        self.board = old_board;
        self.ply -= 1;
        self.ply_from_null = old_ply;

        let len = self.history.len();

        self.sorter.counter_move = if self.ply_from_null > 0 {
            self.history.get(len - 1).map(|t| t.1)
        } else {
            None
        };

        self.sorter.followup_move = if self.ply_from_null > 1 {
            self.history.get(len - 2).map(|t| t.1)
        } else {
            None
        };
    }

    /// Returns true if the tt move has been set
    pub fn found_tt_move(&self) -> bool {
        self.sorter.tt_move.is_some()
    }

    /// Sets the current tt move in the move sorter
    pub fn set_tt_move(&mut self, m: Option<Move>) {
        self.sorter.tt_move = m;
    }

    pub fn update_sorter(&mut self, m: Move, depth: usize, searched: Vec<Move>) {
        self.sorter
            .update(m, depth, self.ply, self.board.side as usize, searched);
    }

    /// Checks whether the current side's king is in check
    pub fn king_in_check(&self) -> bool {
        self.board.checkers != EMPTY_BB
    }

    /// Only king and pawns are on the board. Used to rule out null move pruning
    pub fn only_king_pawns_left(&self) -> bool {
        self.board.big_piece_count == 0
    }

    /// Returns current position's eval
    pub fn evaluate(&self) -> Eval {
        self.nnue_state.evaluate(self.board.side)
    }

    /// Checks if position is a rule-based draw
    pub fn is_draw(&self) -> bool {
        self.board.halfmoves >= 100 || self.is_repetition() || self.insufficient_material()
    }

    /// Check for repetitions in hash history.
    /// We stop at the first occurrence of the position and consider that a draw.
    fn is_repetition(&self) -> bool {
        let rollback = min(self.board.halfmoves, self.ply_from_null);

        self.history
            .iter()
            .rev() // step through history in reverse
            .take(rollback + 1) // only check elements in the rollback
            .skip(1) // first element is opponent, skip.
            .step_by(2) // don't check opponent moves
            .any(|(b, _, _)| b.hash == self.board.hash) // stop at first repetition
    }

    /// Draw by insufficient material (strictly for when it is impossible to mate):
    /// Some of the logic is taken from Tantabus
    fn insufficient_material(&self) -> bool {
        const WHITE_SQUARES: BitBoard = BitBoard(12273903644374837845);
        const CORNERS: BitBoard = BitBoard(9295429630892703873);
        const EDGES: BitBoard = BitBoard(18411139144890810879);

        let kings = self.board.kings();
        let knights = self.board.knights();
        let bishops = self.board.bishops();

        match self.board.occupancy.count_bits() {
            2 => true,
            3 => knights | bishops != EMPTY_BB, // 1 knight or 1 bishop
            4 => {
                let one_each = self.board.side_occupancy[0].count_bits() == 2;
                let knight_count = knights.count_bits();
                let bishop_count = bishops.count_bits();
                let king_in_corner = kings & CORNERS != EMPTY_BB;
                let king_on_edge = kings & EDGES != EMPTY_BB;

                (knight_count == 2 && !king_on_edge) || // knvkn, king not on edge
                (bishop_count == 2 && (
                    (bishops & WHITE_SQUARES).count_bits() != 1 || // same color bishops
                    (one_each && !king_in_corner))) ||  // one bishop each, king not in corner
                (knight_count == bishop_count && one_each && !king_in_corner) // knvkb, king not in corner
            }
            _ => false,
        }
    }
}

/// Game result, used for datagen
/// The bool refers to the game being adjudicated
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug)]
pub enum GameResult {
    Ongoing,
    WhiteWin(bool),
    BlackWin(bool),
    Draw(bool),
}
pub const ADJ: bool = true;
pub const NO_ADJ: bool = false;

/// Datagen-specific implementations
impl Position {
    /// Push the move without updating search-specific data.
    /// Accumulator is refreshed to avoid overflows
    pub fn push_move(&mut self, m: Move) {
        let new = self.board.make_move(m);

        self.sorter = MoveSorter::default();
        self.history.push((self.board, m, self.ply_from_null));

        self.age += 1;
        self.ply = 0;
        self.ply_from_null += 1;

        self.nnue_state.refresh(&new);
        self.board = new;
    }

    /// Checks if the game is over and returns the result
    pub fn check_result(&self) -> GameResult {
        let move_list = self.board.gen_moves::<true>();

        if move_list.is_empty() {
            if self.king_in_check() {
                if self.white_to_move() {
                    GameResult::BlackWin(NO_ADJ)
                } else {
                    GameResult::WhiteWin(NO_ADJ)
                }
            } else {
                GameResult::Draw(NO_ADJ)
            }
        } else if self.is_draw() {
            GameResult::Draw(NO_ADJ)
        } else {
            GameResult::Ongoing
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tables::init_all_tables;

    use super::*;

    #[test]
    fn test_draw() {
        init_all_tables();
        let kbvkn_mate: Position = "fen 5b1K/5k1N/8/8/8/8/8/8 b - - 1 1".parse().unwrap();
        let kbvkn_draw: Position = "fen 8/8/3k4/4n3/8/2KB4/8/8 w - - 0 1".parse().unwrap();
        let krvkn: Position = "fen 8/8/4k3/4n3/8/2KR4/8/8 w - - 0 1".parse().unwrap();

        assert!(!kbvkn_mate.insufficient_material());
        assert!(kbvkn_draw.insufficient_material());
        assert!(!krvkn.insufficient_material());
    }
}
