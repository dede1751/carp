use std::{
    cmp::{max, min},
    str::FromStr,
};

use crate::bitboard::*;
use crate::board::*;
use crate::evaluation::*;
use crate::move_list::*;
use crate::moves::*;
use crate::piece::*;
use crate::sorter::*;
use crate::square::*;
use crate::tables::*;
use crate::zobrist::*;

/// Position, represents a Board's evolution along the search tree.
/// Also incorporates move ordering and various game rules (50mr, draw detection etc)
#[derive(Clone, Debug)]
pub struct Position {
    pub board: Board,
    pub age: u8,
    pub ply: usize,
    pub ply_from_null: usize,
    pub history: Vec<(Board, Move, usize)>,
    sorter: Sorter,
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
            sorter: Sorter::default(),
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

    /// Generate a sorted list of captures
    pub fn generate_captures(&self) -> MoveList {
        let mut move_list = self.board.generate_captures();

        self.sorter.score_captures(self, &mut move_list);
        move_list
    }

    /// Generate a sorted list of moves
    pub fn generate_moves(&self) -> MoveList {
        let mut move_list = self.board.generate_moves();

        self.sorter.score_moves(self, &mut move_list);
        move_list
    }

    /// Makes the given move
    pub fn make_move(&mut self, m: Move) {
        let new = self.board.make_move(m);

        self.history.push((self.board, m, self.ply_from_null));
        self.board = new;
        self.ply += 1;
        self.ply_from_null += 1;
    }

    /// Passes turn to opponent (this resets the ply_from_null clock)
    pub fn make_null(&mut self) {
        let new = self.board.make_null();

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

        self.board = old_board;
        self.ply -= 1;
        self.ply_from_null = old_ply;
    }

    /// Returns true if the tt move has been set
    pub fn found_tt_move(&self) -> bool {
        self.sorter.tt_move.is_some()
    }

    /// Sets the current tt move in the move sorter
    pub fn set_tt_move(&mut self, m: Option<Move>) {
        self.sorter.tt_move = m;
    }

    /// Recovers the move from lookback plies ago. Only goes back to last null move
    /// Panics if lookback > history length or lookback = 0
    pub fn recover_move(&self, lookback: usize) -> Option<Move> {
        if self.ply_from_null >= lookback {
            let index = self.history.len() - lookback;

            Some(self.history.get(index).unwrap().1)
        } else {
            None
        }
    }

    pub fn update_sorter(&mut self, m: Move, depth: usize, searched: Vec<Move>) {
        let counter = self.recover_move(1);
        let followup = self.recover_move(2);
        let side = self.board.side as usize;

        self.sorter
            .update(m, counter, followup, depth, self.ply, side, searched);
    }

    /// Checks whether the current side's king is in check
    pub fn king_in_check(&self) -> bool {
        self.board.checkers != EMPTY_BB
    }

    /// Returns current position's eval
    pub fn evaluate(&self) -> Eval {
        eval(&self.board)
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

                (knight_count == 2 && kings & EDGES != EMPTY_BB)
                    || (bishop_count == 2
                        && ((bishops & WHITE_SQUARES).count_bits() != 1
                            || (one_each && kings & CORNERS != EMPTY_BB)))
                    || (knight_count == 1
                        && bishop_count == 1
                        && one_each
                        && kings & CORNERS != EMPTY_BB)
            }
            _ => false,
        }
    }

    /// Only king and pawns are on the board. Used to rule out null move pruning
    pub fn only_king_pawns_left(&self) -> bool {
        self.board.game_phase == 0
    }
}

/// SEE Implementation
impl Position {
    /// Returns bitboard with all pieces attacking a square
    fn map_all_attackers(&self, square: Square) -> BitBoard {
        let b = self.board;

        b.pieces[WPAWN] & pawn_attacks(square, Color::Black)
            | b.pieces[BPAWN] & pawn_attacks(square, Color::White)
            | b.knights() & knight_attacks(square)
            | (b.bishops() | b.queens()) & bishop_attacks(square, b.occupancy)
            | (b.rooks() | b.queens()) & rook_attacks(square, b.occupancy)
            | b.kings() & king_attacks(square)
    }

    /// Maps sliding attackers assuming the occupancy is that given by the occs bitboard
    /// Used in see to add xray attackers to the attacker bitboard after making a capture.
    /// There is definitely a more efficient way.
    fn remap_xray(&self, square: Square, occs: BitBoard) -> BitBoard {
        let b = self.board;
        let diagonal_sliders = (b.bishops() | b.queens()) & occs;
        let orthogonal_sliders = (b.rooks() | b.queens()) & occs;

        diagonal_sliders & bishop_attacks(square, occs)
            | orthogonal_sliders & rook_attacks(square, occs)
    }

    /// Returns the least valuable of the attackers within the attacker map
    fn get_lva(&self, attackers: BitBoard, side: Color) -> Option<(Square, Piece)> {
        for piece in PIECES[side as usize] {
            let squares = attackers & self.board.pieces[piece as usize];

            if squares != EMPTY_BB {
                return Some((squares.lsb(), piece));
            }
        }

        None
    }

    /// Returns the static exchange evaluation of the given move in the position
    /// Note that the see score is a much less usesful sorting metric compared to mvv-lva. We only
    /// use it when a capture is losing material to quantify how much, not when it's winning.
    ///
    /// TODO: move to a simple boolean see, since the value returned is not used
    pub fn see(&self, m: Move) -> i16 {
        const SEE_VALUES: [i16; PIECE_COUNT] = [1, 1, 3, 3, 3, 3, 5, 5, 9, 9, 20, 20];
        let mut swap_list: [i16; 32] = [0; 32];
        swap_list[0] = SEE_VALUES[m.get_capture() as usize];

        let mut swap_piece = m.get_piece();
        let mut src = m.get_src();
        let tgt = m.get_tgt();

        let possible_xray =
            self.board.pawns() | self.board.bishops() | self.board.rooks() | self.board.queens();

        let mut side = !self.board.side;
        let mut occs = self.board.occupancy;
        let mut attackers = self.map_all_attackers(tgt);

        let mut depth = 1;
        loop {
            // score assuming capturing piece is lost afterwards
            swap_list[depth] = SEE_VALUES[swap_piece as usize] - swap_list[depth - 1];

            // early stand pat pruning
            if max(-swap_list[depth - 1], swap_list[depth]) < 0 {
                break;
            }

            // remove capturing piece and add back xray attackers
            attackers = attackers.pop_bit(src);
            occs = occs.pop_bit(src);
            if possible_xray.get_bit(src) {
                attackers |= self.remap_xray(tgt, occs);
            }

            match self.get_lva(attackers, side) {
                Some((sq, p)) => {
                    src = sq;
                    swap_piece = p;

                    side = !side;
                    depth += 1;
                }

                None => break,
            }
        }

        // negamax the results
        for d in (1..depth).rev() {
            swap_list[d - 1] = -max(swap_list[d], -swap_list[d - 1]);
        }
        swap_list[0]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_see_helpers() {
        init_all_tables();
        let pos1: Position = "fen 1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1"
            .parse()
            .unwrap();
        let pos2: Position = "fen 1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1"
            .parse()
            .unwrap();

        let att1 = pos1.map_all_attackers(Square::E5);
        let att2 = pos2.map_all_attackers(Square::E5);

        println!("{}\n{}\n{}\n{}", pos1.board, att1, pos2.board, att2);

        assert!(att1.get_bit(Square::E1));
        assert!(!att1.get_bit(Square::D8));

        assert!(att2.get_bit(Square::E2));
        assert!(!att2.get_bit(Square::E1));

        let occs = pos2.board.occupancy.pop_bit(Square::E2);
        let remap = pos2.remap_xray(Square::E5, occs);

        println!("{remap}");

        assert!(remap.get_bit(Square::E1));
        assert!(!remap.get_bit(Square::E2));
    }

    #[test]
    fn test_see() {
        init_all_tables();
        let pos1: Position = "fen 1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1"
            .parse()
            .unwrap();
        let pos2: Position = "fen 1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1"
            .parse()
            .unwrap();
        let pos3: Position =
            "fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
                .parse()
                .unwrap();

        println!("{}\n{}\n{}", pos1.board, pos2.board, pos3.board);

        let m1 = pos1.board.find_move("e1e5").unwrap();
        let m2 = pos2.board.find_move("d3e5").unwrap();
        let m3 = pos3.board.find_move("g2h3").unwrap();

        assert_eq!(pos1.see(m1), 1);
        assert_eq!(pos2.see(m2), -2);
        assert_eq!(pos3.see(m3), 1);
    }
}
