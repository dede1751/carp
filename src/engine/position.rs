use std::{cmp::min, str::FromStr};

use crate::chess::{bitboard::*, board::*, move_list::*, moves::*, piece::*};
use crate::engine::{move_sorter::*, nnue::*, search_info::*, search_params::*};

/// Position, represents a Board's evolution along the game tree.
/// Also incorporates move ordering and various game rules (50mr, draw detection etc)
#[derive(Clone, Debug)]
pub struct Position {
    pub board: Board,
    history: Vec<Board>,
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

        let mut history = Vec::new();
        if let Some("moves") = tokens.next() {
            for move_str in tokens {
                let m = board.find_move(move_str);

                match m {
                    Some(m) => {
                        let new = board.make_move(m);
                        history.push(board);
                        board = new;
                    }
                    None => eprintln!("Move is not legal!"),
                };
            }
        };

        let nnue_state = NNUEState::from_board(&board);

        Ok(Position {
            board,
            history,
            nnue_state,
        })
    }
}

/// Default position is startpos
impl Default for Position {
    fn default() -> Self {
        "startpos".parse().unwrap()
    }
}

impl Position {
    /// Generate and sort either all moves or only captures in a position.
    /// In case of only captures, ply is superfluous.
    pub fn generate_moves<const QUIETS: bool>(&self, info: &mut SearchInfo) -> MoveList {
        let mut move_list = self.board.gen_moves::<QUIETS>();

        MoveSorter::score_moves::<QUIETS>(info, &self.board, &mut move_list);
        move_list
    }

    /// Makes the given move within the game tree
    /// We use std::mem::replace to avoid cloning the board
    pub fn make_move(&mut self, m: Move, info: &mut SearchInfo) {
        let new = self.board.make_move_nnue(m, &mut self.nnue_state);
        let old = std::mem::replace(&mut self.board, new);

        let piece = old.piece_at(m.get_src());
        self.history.push(old);

        info.push_move(piece, m);
    }

    /// Passes turn to opponent (this resets the ply_from_null clock in the search info)
    pub fn make_null(&mut self, info: &mut SearchInfo) {
        let new = self.board.make_null();
        let old = std::mem::replace(&mut self.board, new);

        self.nnue_state.push();
        self.history.push(old);

        info.push_null();
    }

    /// Pops the current board, going back to previous history entry
    /// Panics if the history vector is empty!
    pub fn undo_move(&mut self, info: &mut SearchInfo) {
        let old_board = self.history.pop().unwrap();
        self.nnue_state.pop();
        self.board = old_board;

        info.pop_move();
    }

    /// Returns true if it's white to move
    pub fn white_to_move(&self) -> bool {
        self.board.side == Color::White
    }

    /// Checks whether the current side's king is in check
    pub fn king_in_check(&self) -> bool {
        self.board.checkers != EMPTY_BB
    }

    /// Only king and pawns are on the board for the side to move. Possible Zugzwang.
    pub fn only_king_pawns_left(&self) -> bool {
        (self.board.own_occupancy() ^ self.board.own_king() ^ self.board.own_pawns()) == EMPTY_BB
    }

    /// Checks if position is a rule-based draw
    pub fn is_draw(&self, ply_from_null: usize) -> bool {
        self.board.halfmoves >= 100
            || self.is_repetition(ply_from_null)
            || self.insufficient_material()
    }

    /// Return the NNUE evaluation of the current position
    /// We scale the evaluation by the total material on the board
    pub fn evaluate(&self) -> Eval {
        let eval = self.nnue_state.evaluate(self.board.side);

        #[rustfmt::skip]
        let total_material =
            self.board.knights().count_bits() as Eval * PIECE_VALUES[Piece::WK as usize] +
            self.board.bishops().count_bits() as Eval * PIECE_VALUES[Piece::WB as usize] +
            self.board.rooks().count_bits() as Eval   * PIECE_VALUES[Piece::WR as usize] +
            self.board.queens().count_bits() as Eval  * PIECE_VALUES[Piece::WQ as usize];

        (eval * (700 + total_material / 32)) / 1024
    }

    /// Check for repetitions in hash history.
    /// We stop at the first occurrence of the position and consider that a draw.
    fn is_repetition(&self, ply_from_null: usize) -> bool {
        let rollback = min(self.board.halfmoves, ply_from_null);

        self.history
            .iter()
            .rev() // step through history in reverse
            .take(rollback + 1) // only check elements in the rollback
            .skip(1) // first element is opponent, skip.
            .step_by(2) // don't check opponent moves
            .any(|b| b.hash == self.board.hash) // stop at first repetition
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
                (knight_count == 1 && bishop_count == 1 && one_each && !king_in_corner)
                // knvkb, king not in corner
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
        self.nnue_state.refresh(&new);

        let old = std::mem::replace(&mut self.board, new);
        self.history.push(old);
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
        } else if self.is_draw(self.board.halfmoves) {
            GameResult::Draw(NO_ADJ)
        } else {
            GameResult::Ongoing
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::chess::init_all_tables;

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
