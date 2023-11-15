use crate::{
    move_picker::MovePicker,
    nnue::NNUEState,
    search_params::*,
    syzygy::probe::{TB, WDL},
    thread::Thread,
};
use chess::{
    bitboard::BitBoard,
    board::Board,
    board::{bishop_attacks, rook_attacks},
    moves::{Move, MoveType},
    piece::{Color, Piece},
};

/// Position, represents a Board's evolution along the game tree.
/// Also incorporates move ordering and various game rules (50mr, draw detection etc)
#[derive(Clone, Debug)]
pub struct Position {
    pub board: Board,
    history: Vec<Board>,
    nnue_state: Box<NNUEState>,
}

/// Get position from uci position string
impl std::str::FromStr for Position {
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

        Ok(Self {
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
    /// Produce a move picker for the current position
    pub fn gen_moves<const QUIETS: bool>(
        &self,
        tt_move: Option<Move>,
        see_threshold: Eval,
    ) -> MovePicker<QUIETS> {
        let move_list = self.board.gen_moves::<QUIETS>();

        MovePicker::<QUIETS>::new(move_list, tt_move, see_threshold)
    }

    /// Makes the given move within the game tree
    /// We use std::mem::replace to avoid cloning the board
    pub fn make_move(&mut self, m: Move, t: &mut Thread) {
        self.nnue_state.push();
        self.nnue_state.update(m, &self.board);

        let new = self.board.make_move(m);
        let old = std::mem::replace(&mut self.board, new);

        let piece = old.piece_at(m.get_src());
        self.history.push(old);
        t.push_move(piece, m);
    }

    /// Passes turn to opponent (this resets the ply_from_null clock in the thread)
    /// Calling this when in check breaks the game state!
    pub fn make_null(&mut self, t: &mut Thread) {
        self.nnue_state.push();
        let new = self.board.make_null();
        let old = std::mem::replace(&mut self.board, new);

        self.history.push(old);
        t.push_null();
    }

    /// Pops the current board, going back to previous history entry
    /// Panics if the history vector is empty!
    pub fn undo_move(&mut self, t: &mut Thread) {
        self.board = self.history.pop().unwrap();
        self.nnue_state.pop();
        t.pop_move();
    }

    /// Returns true if it's white to move
    pub fn white_to_move(&self) -> bool {
        self.board.side == Color::White
    }

    /// Checks whether the current side's king is in check
    pub fn king_in_check(&self) -> bool {
        self.board.checkers != BitBoard::EMPTY
    }

    /// Only king and pawns are on the board for the side to move. Possible Zugzwang.
    pub fn only_king_pawns_left(&self) -> bool {
        (self.board.own_occupancy() ^ self.board.own_king() ^ self.board.own_pawns())
            == BitBoard::EMPTY
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

    /// Check for repetitions in hash history (twofold)
    fn is_repetition(&self, ply_from_null: usize) -> bool {
        let rollback = 1 + ply_from_null.min(self.board.halfmoves);

        // Rollback == 1 implies we only look at the opponent's position.
        if rollback == 1 {
            return false;
        }

        self.history
            .iter()
            .rev() // step through history in reverse
            .take(rollback) // only check elements within rollback
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

        match self.board.occupancy().count_bits() {
            2 => true,
            3 => knights | bishops != BitBoard::EMPTY, // 1 knight or 1 bishop
            4 => {
                let one_each = self.board.own_occupancy().count_bits() == 2;
                let knight_count = knights.count_bits();
                let bishop_count = bishops.count_bits();
                let king_in_corner = kings & CORNERS != BitBoard::EMPTY;
                let king_on_edge = kings & EDGES != BitBoard::EMPTY;

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

    /// Checks if the static exchange after a move is enough to beat the given threshold
    /// This can be used for both captures and quiet moves.
    /// This implementation is basically that seen in Viri, which in turn is that of Ethereal
    pub fn see(&self, m: Move, threshold: Eval) -> bool {
        let b = &self.board;
        let src = m.get_src();
        let tgt = m.get_tgt();
        let mt = m.get_type();

        // Castling cannot have bad SEE, since all squares the king passes through are not attacked
        if mt == MoveType::Castle {
            return true;
        }

        // Piece being swapped off is the promoted piece
        let victim = if mt.is_promotion() {
            mt.get_promotion(b.side)
        } else {
            b.piece_at(src)
        };

        // Get the static move value (also works for quiets)
        let mut move_value = if mt.is_capture() {
            if mt == MoveType::EnPassant {
                PIECE_VALUES[Piece::WP as usize]
            } else {
                PIECE_VALUES[b.piece_at(tgt) as usize]
            }
        } else {
            0
        };
        if mt.is_promotion() {
            move_value += PIECE_VALUES[victim as usize] - PIECE_VALUES[0];
        }

        // Lose if the balance is already in our opponent's favor and it's their turn
        let mut balance = move_value - threshold;
        if balance < 0 {
            return false;
        }

        // Win if the balance is still in our favor even if we lose the capturing piece
        // This ensures a positive SEE in case of even trades.
        balance -= PIECE_VALUES[victim as usize];
        if balance >= 0 {
            return true;
        }

        let diagonal_sliders = b.bishops() | b.queens();
        let orthogonal_sliders = b.rooks() | b.queens();

        // Updated occupancy map after capture
        let mut occs = b.occupancy().pop_bit(src).set_bit(tgt);
        if mt == MoveType::EnPassant {
            let ep_tgt = b.en_passant.unwrap().forward(!b.side); // guaranteed to be Some
            occs = occs.pop_bit(ep_tgt);
        }

        // Get all pieces covering the exchange square and start exchanging
        let mut attackers = b.map_all_attackers(tgt, occs) & occs;
        let mut side_to_move = !b.side;

        loop {
            // SEE terminates when no recapture is possible.
            let own_attackers = attackers & b.side_bb[side_to_move as usize];
            if own_attackers == BitBoard::EMPTY {
                break;
            }

            // Get the least valuable attacker and simulate the recapture
            let (attacker_square, attacker) = b.get_lva(own_attackers, side_to_move).unwrap(); // attackers are at least one
            occs = occs.pop_bit(attacker_square);

            // Diagonal recaptures uncover bishops/queens
            if attacker.is_pawn() || attacker.is_bishop() || attacker.is_queen() {
                attackers |= bishop_attacks(tgt, occs) & diagonal_sliders;
            }

            // Orthogonal recaptures uncover rooks/queens
            if attacker.is_rook() || attacker.is_queen() {
                attackers |= rook_attacks(tgt, occs) & orthogonal_sliders;
            }
            attackers &= occs; // ignore pieces already "used up"

            // Negamax the balance, cutoff if losing our attacker would still win the exchange
            side_to_move = !side_to_move;
            balance = -balance - 1 - PIECE_VALUES[attacker as usize];
            if balance >= 0 {
                // If the recapturing piece is a king, and the opponent has another attacker,
                // a positive balance should not translate to an exchange win.
                if attacker.is_king()
                    && attackers & b.side_bb[side_to_move as usize] != BitBoard::EMPTY
                {
                    return b.side == side_to_move;
                }

                break;
            }
        }

        // We win the exchange if we are not the one who should recapture
        b.side != side_to_move
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
    /// Returns the number of plies in the game
    pub fn ply(&self) -> usize {
        self.history.len()
    }

    /// Push the move without updating search-specific data.
    /// Accumulator is refreshed to avoid overflows
    pub fn push_move(&mut self, m: Move) {
        let new = self.board.make_move(m);
        self.nnue_state.refresh(&new);

        let old = std::mem::replace(&mut self.board, new);
        self.history.push(old);
    }

    /// Checks if the game is over and returns the result.
    /// If available, adjudicates using the TBs.
    pub fn check_result(&self, tb: TB) -> GameResult {
        if let Some(result) = tb.probe_root(&self.board) {
            let wtm = self.white_to_move();

            return if result.wdl == WDL::Draw {
                GameResult::Draw(ADJ)
            } else if (result.wdl == WDL::Win && wtm) || (result.wdl == WDL::Loss && !wtm) {
                GameResult::WhiteWin(ADJ)
            } else {
                GameResult::BlackWin(ADJ)
            };
        }

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
    use super::*;

    #[test]
    fn test_draw() {
        let kbvkn_mate: Position = "fen 5b1K/5k1N/8/8/8/8/8/8 b - - 1 1".parse().unwrap();
        let kbvkn_draw: Position = "fen 8/8/3k4/4n3/8/2KB4/8/8 w - - 0 1".parse().unwrap();
        let krvkn: Position = "fen 8/8/4k3/4n3/8/2KR4/8/8 w - - 0 1".parse().unwrap();

        assert!(!kbvkn_mate.insufficient_material());
        assert!(kbvkn_draw.insufficient_material());
        assert!(!krvkn.insufficient_material());
    }

    #[test]
    fn test_see() {
        #[rustfmt::skip]
        const SEE_SUITE: [(&str, &str, Eval, bool); 13] = [
            ("fen 1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1", "e1e5", 0, true),
            ("fen 1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1", "d3e5", 0, false),
            ("fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", "g2h3", 0, true),
            ("fen k3r3/8/8/4p3/8/2B5/1B6/K7 w - - 0 1", "c3e5", 0, true),
            ("fen 4kbnr/p1P4p/b1q5/5pP1/4n3/5Q2/PP1PPP1P/RNB1KBNR w KQk f6 0 1", "g5f6", 0, true),
            ("fen 6k1/1pp4p/p1pb4/6q1/3P1pRr/2P4P/PP1Br1P1/5RKN w - - 0 1", "f1f4", 0, false),
            ("fen 6RR/4bP2/8/8/5r2/3K4/5p2/4k3 w - - 0 1", "f7f8q", 0, true),
            ("fen r1bqk1nr/pppp1ppp/2n5/1B2p3/1b2P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1", "e1g1", 0, true),
            ("fen 4kbnr/p1P1pppp/b7/4q3/7n/8/PPQPPPPP/RNB1KBNR w KQk - 0 1", "c7c8q", 0, true),
            ("fen 4kbnr/p1P1pppp/b7/4q3/7n/8/PP1PPPPP/RNBQKBNR w KQk - 0 1", "c7c8q", 0, false),
            ("fen 3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - - 0 1", "d3d4", 0, false),
            ("fen 5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - - 0 1", "d6f4", 0, false),
            ("fen 5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - - 0 1", "d6f4", -108, true),
        ];

        for (b, m, t, r) in SEE_SUITE {
            let pos: Position = b.parse().unwrap();
            let m = pos.board.find_move(m).unwrap();

            println!("Move: {m}{}", pos.board);
            assert_eq!(pos.see(m, t), r);
        }
    }
}
