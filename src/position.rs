/// Position, represents a Board's evolution along the search tree.
/// Also incorporates move ordering and various game rules (50mr, draw detection etc)
use std::{
    cmp::{max, min},
    str::FromStr,
};

use crate::{
    bitboard::*, board::*, evaluation::*, move_list::*, moves::*, piece::*, search::MAX_DEPTH,
    square::*, tables::*, zobrist::*,
};

#[derive(Clone, Debug)]
pub struct Position {
    pub board: Board,
    pub age: u8,
    pub ply: usize,
    ply_from_null: usize,
    history: Vec<(Board, usize)>,
    killer_moves: [[Move; MAX_KILLERS]; MAX_DEPTH],
    history_moves: [[i16; SQUARE_COUNT]; PIECE_COUNT],
    tt_move: Option<Move>,
}

impl FromStr for Position {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = s.split_whitespace();

        let mut board: Board = match tokens.next() {
            Some("startpos") => Board::default(),
            Some("fen") => {
                let fen = &tokens.clone().take(6).collect::<Vec<&str>>().join(" ")[..];

                // advance iterator
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
                        history.push((board, ply_from_null));

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
            killer_moves: [[NULL_MOVE; MAX_KILLERS]; MAX_DEPTH as usize],
            history_moves: [[0; SQUARE_COUNT]; PIECE_COUNT],
            tt_move: None,
        };

        Ok(res)
    }
}

/// Startpos
impl Default for Position {
    fn default() -> Self {
        "fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            .parse()
            .unwrap()
    }
}

impl Position {
    pub fn hash(&self) -> ZHash {
        self.board.hash
    }

    /// Generate a sorted list of captures
    pub fn generate_captures(&self) -> MoveList {
        let mut move_list = self.board.generate_captures();

        self.score_captures(&mut move_list);
        move_list
    }

    /// Generate a sorted list of moves
    pub fn generate_moves(&self) -> MoveList {
        let mut move_list = self.board.generate_moves();

        self.score_moves(&mut move_list);
        move_list
    }

    /// Makes the given move
    pub fn make_move(&mut self, m: Move) {
        let new = self.board.make_move(m);

        self.history.push((self.board, self.ply_from_null));
        self.board = new;
        self.ply += 1;
        self.ply_from_null += 1;
    }

    /// Passes turn to opponent
    pub fn make_null(&mut self) {
        let mut new = self.board.clone();

        new.side = !new.side;
        new.hash.toggle_side();

        if let Some(square) = new.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        self.history.push((self.board, self.ply_from_null));
        self.board = new;
        self.ply += 1;
        self.ply_from_null = 0;
    }

    /// Pops the current board, going back to previous history entry
    /// Panics if the history vector is empty!
    pub fn undo_move(&mut self) {
        let (old_board, old_ply) = self.history.pop().unwrap();

        self.board = old_board;
        self.ply -= 1;
        self.ply_from_null = old_ply;
    }

    /// Checks whether the current side's king is in check
    ///
    /// Works on the basic idea that, if a certain square is attacked, if we put the attacking piece
    /// on the attacked square it will attack its old square. Hence we generate attacks on the
    /// target square for each piece, and check whether they land on any of the pieces stored
    /// in the board representation.
    pub fn king_in_check(&self) -> bool {
        let b = self.board;
        let square = b.own_king().lsb();

        b.opp_pawns()   & pawn_attacks(square, b.side)             != EMPTY_BB || // pawns
        b.opp_knights() & knight_attacks(square)                   != EMPTY_BB || // knights
        b.opp_queen_bishop() & bishop_attacks(square, b.occupancy) != EMPTY_BB || // bishops + queens
        b.opp_queen_rook()   & rook_attacks(square, b.occupancy)   != EMPTY_BB || // rooks + queens
        b.opp_king() & king_attacks(square)                        != EMPTY_BB // kings
    }

    /// Returns current position's eval
    pub fn evaluate(&self) -> Eval {
        evaluate(&self.board)
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
            .any(|(b, _)| b.hash == self.board.hash) // stop at first repetition
    }

    /// Draw by insufficient material (strictly for when it is impossible to mate):
    ///     - King vs King
    ///     - King vs King + Bishop
    ///     - King vs King + Knight
    ///     - King + Bishop vs King + Bishop
    fn insufficient_material(&self) -> bool {
        match self.board.occupancy.count_bits() {
            2 => true,
            3 => self.board.knights() | self.board.bishops() != EMPTY_BB, // 1 knight or 1 bishop
            4 => {
                self.board.bishops().count_bits() == 2 &&                // opposite color bishops
                (self.board.bishops() & WHITE_SQUARES).count_bits() == 1
            }
            _ => false,
        }
    }

    /// Only king and pawns are on the board. Used to rule out null move pruning
    pub fn only_king_pawns_left(&self) -> bool {
        self.board.occupancy & !self.board.kings() & !self.board.pawns() == EMPTY_BB
    }
}

/// Move Scoring
///
/// * TT moves when found are scored best.
/// * Queen promotions go to the top of the list, all other promotions are considered last.
/// * Good captures are evaluated with MVVLVA values [100, 605]
/// * Bad captures are evaluated after killer and castling moves.
/// * Quiet moves are evaluated in the following order:
///   1) Killer moves are awarded the most points (3 for first and 2 for second)
///   2) Castling always comes after killers and before all other quiets and bad captures
///   3) Remaining quiets all have negative scores, obtained from the history score - 30000.
const TT_SCORE: i16 = 2000;
const FIRST_KILLER_SCORE: i16 = 3;
const SECOND_KILLER_SCORE: i16 = 2;
const MAX_KILLERS: usize = 2;
const CASTLE_SCORE: i16 = 1;
const WORST: i16 = -30000;

const PROMOTION_SCORES: [i16; PIECE_COUNT] = [
    0, 0, WORST, WORST, WORST, WORST, WORST, WORST, 1000, 1000, 0, 0,
];

/// Static Exchange Evaluation
const SEE_VALUES: [i16; PIECE_COUNT] = [
    100, 100, 300, 300, 300, 300, 500, 500, 1000, 1000, 5000, 5000,
];

#[rustfmt::skip]
const MVV_LVA: [[i16; PIECE_COUNT]; PIECE_COUNT] = [
//     WP   BP   WN   BN   WB   BB   WR   BR   WQ   BQ   WK   BK 
    [ 105, 105, 205, 205, 305, 305, 405, 405, 505, 505, 605, 605 ], // WP
    [ 105, 105, 205, 205, 305, 305, 405, 405, 505, 505, 605, 605 ], // BP
    [ 104, 104, 204, 204, 304, 304, 404, 404, 504, 504, 604, 604 ], // WN
    [ 104, 104, 204, 204, 304, 304, 404, 404, 504, 504, 604, 604 ], // BN
    [ 103, 103, 203, 203, 303, 303, 403, 403, 503, 503, 603, 603 ], // WB
    [ 103, 103, 203, 203, 303, 303, 403, 403, 503, 503, 603, 603 ], // BB
    [ 102, 102, 202, 202, 302, 302, 402, 402, 502, 502, 602, 602 ], // WR
    [ 102, 102, 202, 202, 302, 302, 402, 402, 502, 502, 602, 602 ], // BR
    [ 101, 101, 201, 201, 301, 301, 401, 401, 501, 501, 601, 601 ], // WQ
    [ 101, 101, 201, 201, 301, 301, 401, 401, 501, 501, 601, 601 ], // BQ
    [ 100, 100, 200, 200, 300, 300, 400, 400, 500, 500, 600, 600 ], // WK
    [ 100, 100, 200, 200, 300, 300, 400, 400, 500, 500, 600, 600 ], // BK
];

impl Position {
    /// Sets the current tt move
    pub fn set_tt_move(&mut self, m: Option<Move>) {
        self.tt_move = m;
    }

    /// Update killer and history values for sorting quiet moves
    pub fn update_sorter(&mut self, m: Move, depth: usize) {
        // killer moves
        let first_killer = self.killer_moves[self.ply as usize][0];

        if first_killer != m {
            self.killer_moves[self.ply as usize][1] = first_killer;
            self.killer_moves[self.ply as usize][0] = m;
        }

        // history moves
        let p = m.get_piece() as usize;
        let sq = m.get_tgt() as usize;
        self.history_moves[p][sq] += (depth * depth) as i16;

        // reset history values when surpassing captures/promotions/killers
        if self.history_moves[p][sq] >= -WORST {
            for p in ALL_PIECES {
                for sq in ALL_SQUARES {
                    self.history_moves[p as usize][sq as usize] >>= 1;
                }
            }
        }
    }

    /// Score individual captures.
    fn score_capture(&self, m: Move) -> i16 {
        let promote_score = PROMOTION_SCORES[m.get_promotion() as usize];

        if promote_score == 0 && self.see(m) >= 0 {
            MVV_LVA[m.get_piece() as usize][m.get_capture() as usize]
        } else {
            promote_score
        }
    }

    /// Score assuming all moves are captures
    fn score_captures(&self, move_list: &mut MoveList) {
        for i in 0..move_list.len() {
            move_list.scores[i] = self.score_capture(move_list.moves[i])
        }
    }

    /// Score any type of move
    fn score_move(&self, m: Move) -> i16 {
        if m.is_promotion() {
            PROMOTION_SCORES[m.get_promotion() as usize]
        } else if m.is_capture() {
            if self.see(m) >= 0 {
                MVV_LVA[m.get_piece() as usize][m.get_capture() as usize]
            } else {
                0
            }
        } else {
            if m.is_castle() {
                CASTLE_SCORE
            } else if m == self.killer_moves[self.ply][0] {
                FIRST_KILLER_SCORE
            } else if m == self.killer_moves[self.ply][1] {
                SECOND_KILLER_SCORE
            } else {
                // add history score when it's not a castling or killer move
                WORST + self.history_moves[m.get_piece() as usize][m.get_tgt() as usize]
            }
        }
    }

    /// Sort all moves in the movelist
    fn score_moves(&self, move_list: &mut MoveList) {
        match self.tt_move {
            Some(tt_move) => {
                for i in 0..move_list.len() {
                    let m = move_list.moves[i];

                    move_list.scores[i] = if m == tt_move {
                        TT_SCORE
                    } else {
                        self.score_move(m)
                    }
                }
            }

            None => {
                for i in 0..move_list.len() {
                    move_list.scores[i] = self.score_move(move_list.moves[i]);
                }
            }
        };
    }

    /// Returns bitboard with all pieces attacking a square
    fn map_all_attackers(&self, square: Square) -> BitBoard {
        let b = self.board;

        b.pieces[WPAWN] & pawn_attacks(square, Color::Black)             | // own pawns
        b.pieces[BPAWN] & pawn_attacks(square, Color::White)             | // opp pawns
        b.knights() & knight_attacks(square)                             | // knights
        (b.bishops() | b.queens()) & bishop_attacks(square, b.occupancy) | // bishops + queens
        (b.rooks()   | b.queens()) & rook_attacks(square, b.occupancy)   | // rooks + queens
        b.kings() & king_attacks(square) // kings
    }

    /// Maps sliding attackers assuming the occupancy is that given by the occs bitboard
    /// Used in see to add xray attackers to the attacker bitboard after making a capture.
    /// There is definitely a more efficient way.
    fn remap_xray(&self, square: Square, occs: BitBoard) -> BitBoard {
        let b = self.board;
        let diagonal_sliders = (b.bishops() | b.queens()) & occs;
        let orthogonal_sliders = (b.rooks() | b.queens()) & occs;

        diagonal_sliders & bishop_attacks(square, occs) | // bishops + queens
        orthogonal_sliders & rook_attacks(square, occs) // rooks + queens
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
    fn see(&self, m: Move) -> i16 {
        let mut swap_list: [i16; 32] = [0; 32];
        swap_list[0] = SEE_VALUES[m.get_capture() as usize];

        let mut swap_piece = m.get_piece();
        let mut src = m.get_src();

        let possible_xray =
            self.board.pawns() | self.board.bishops() | self.board.rooks() | self.board.queens();

        let mut side = !self.board.side;
        let mut occs = self.board.occupancy;
        let mut attackers = self.map_all_attackers(m.get_tgt());

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
                attackers |= self.remap_xray(src, occs);
            }

            // get the next attacker
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

        println!("{}", remap);

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

        println!("{}\n{}", pos1.board, pos2.board);

        let m1 = pos1.board.find_move("e1e5").unwrap();
        let m2 = pos2.board.find_move("d3e5").unwrap();

        assert_eq!(pos1.see(m1), 100);
        assert_eq!(pos2.see(m2), -200);
    }

    #[test]
    fn test_sorter() {
        init_all_tables();
        let pos: Position =
            "fen rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"
                .parse()
                .unwrap();
        let move_list = pos.generate_moves();

        // Move list is ordered correctly
        let mut prev = -WORST;
        for (_, score) in move_list {
            assert!(prev >= score);
            prev = score;
        }
    }
}
