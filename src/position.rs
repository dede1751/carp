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
use crate::search::*;
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
    ply_from_null: usize,
    history: Vec<(Board, Move, usize)>,
    killer_moves: [[Move; MAX_KILLERS]; MAX_DEPTH], // [ply][num_killer]
    history_moves: [[[i32; SQUARE_COUNT]; SQUARE_COUNT]; 2], // [color][from][to]
    counter_moves: Box<[[[[i32; SQUARE_COUNT]; SQUARE_COUNT]; SQUARE_COUNT]; PIECE_COUNT]>, // [piece+col][to][from][to]
    followup_moves: Box<[[[[i32; SQUARE_COUNT]; SQUARE_COUNT]; SQUARE_COUNT]; PIECE_COUNT]>, // [piece+col][to][from][to]
    tt_move: Option<Move>,
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
            killer_moves: [[NULL_MOVE; MAX_KILLERS]; MAX_DEPTH as usize],
            history_moves: [[[0; SQUARE_COUNT]; SQUARE_COUNT]; 2],
            counter_moves: Box::new(
                [[[[0; SQUARE_COUNT]; SQUARE_COUNT]; SQUARE_COUNT]; PIECE_COUNT],
            ),
            followup_moves: Box::new(
                [[[[0; SQUARE_COUNT]; SQUARE_COUNT]; SQUARE_COUNT]; PIECE_COUNT],
            ),
            tt_move: None,
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

                (knight_count == 2 && kings & EDGES != EMPTY_BB) // two knights, king not on edge
                    || (bishop_count == 2
                        && ((bishops & WHITE_SQUARES).count_bits() != 1 // same color bishops
                            || (one_each && kings & CORNERS != EMPTY_BB))) // opposite color, king not in corner
                    || (one_each && kings & CORNERS != EMPTY_BB) // bishop and knight, king not in corner
            }
            _ => false,
        }
    }

    /// Only king and pawns are on the board. Used to rule out null move pruning
    pub fn only_king_pawns_left(&self) -> bool {
        self.board.game_phase == 0
    }
}

///     Move Scoring
/// 300         -> TT Move
/// 200         -> queen promotion
/// 110:165     -> good and equal captures according to MVV-LVA
/// 110         -> enpassant
/// 102:103     -> second and first killer
/// 101         -> castling move
///  10: 65     -> bad captures according to MVV-LVA
///   0:-98304  -> quiet moves according to history score
const MAX_KILLERS: usize = 2;

const TT_SCORE: i32 = 300;

pub const GOOD_CAPTURE_OFFSET: i32 = 100;
const EP_SCORE: i32 = GOOD_CAPTURE_OFFSET + 10;
const FIRST_KILLER_SCORE: i32 = GOOD_CAPTURE_OFFSET + 3;
const SECOND_KILLER_SCORE: i32 = GOOD_CAPTURE_OFFSET + 2;
const CASTLE_SCORE: i32 = GOOD_CAPTURE_OFFSET + 1;

pub const HISTORY_OFFSET: i32 = -(3 * 16384);
const WORST: i32 = -(6 * 16384); // each history can be at the least -16384, start at history offset

const PROMOTION_SCORES: [i32; PIECE_COUNT] = [
    0, 0, WORST, WORST, WORST, WORST, WORST, WORST, 200, 200, 0, 0,
];

/// Static Exchange Evaluation piece scores
const SEE_VALUES: [i32; PIECE_COUNT] = [1, 1, 3, 3, 3, 3, 5, 5, 9, 9, 20, 20];

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

impl Position {
    pub fn found_tt_move(&self) -> bool {
        self.tt_move.is_some()
    }

    /// Sets the current tt move
    pub fn set_tt_move(&mut self, m: Option<Move>) {
        self.tt_move = m;
    }

    /// Taper history so that it's bounded to 32 * 512 = 16384 (and -16384)
    /// Discussed here:
    /// http://www.talkchess.com/forum3/viewtopic.php?f=7&t=76540
    fn tapered_history(bonus: i32, old: i32) -> i32 {
        old + 16 * bonus - old * bonus / 512
    }

    /// Update killer and history values for sorting quiet moves
    pub fn update_sorter(&mut self, m: Move, depth: usize, searched: Vec<Move>) {
        let first_killer = self.killer_moves[self.ply as usize][0];

        if first_killer != m {
            self.killer_moves[self.ply as usize][1] = first_killer;
            self.killer_moves[self.ply as usize][0] = m;
        }

        // leaves can introduce a lot of random noise to history scores, don't consider them
        if depth < 3 {
            return;
        }

        // history bonus is Stockfish's "gravity"
        let bonus = ((depth * depth) as i32).clamp(-400, 400);

        self.update_history(m, bonus, &searched);

        // countermove history
        if self.ply_from_null > 0 {
            let prev = self.history.last().unwrap().1; // impossible panic
            self.update_counters(m, prev, bonus, &searched);

            // followup history
            if self.ply_from_null > 1 {
                let prev = self.history.get(self.history.len() - 2).unwrap().1; // impossible panic
                self.update_followup(m, prev, bonus, &searched);
            }
        }
    }

    fn update_history(&mut self, curr: Move, bonus: i32, searched: &Vec<Move>) {
        let col = self.board.side as usize;

        for m in searched {
            let src = m.get_src() as usize;
            let tgt = m.get_tgt() as usize;
            let old_hist = self.history_moves[col][src][tgt];

            self.history_moves[col][src][tgt] = Self::tapered_history(-bonus, old_hist);
        }

        let src = curr.get_src() as usize;
        let tgt = curr.get_tgt() as usize;
        let old_hist = self.history_moves[col][src][tgt];

        self.history_moves[col][src][tgt] = Self::tapered_history(bonus, old_hist);
    }

    fn update_counters(&mut self, curr: Move, prev: Move, bonus: i32, searched: &Vec<Move>) {
        let prev_p = prev.get_piece() as usize;
        let prev_tgt = prev.get_tgt() as usize;

        for m in searched {
            let src = m.get_src() as usize;
            let tgt = m.get_tgt() as usize;
            let old_cmh = self.counter_moves[prev_p][prev_tgt][src][tgt];

            self.counter_moves[prev_p][prev_tgt][src][tgt] = Self::tapered_history(-bonus, old_cmh);
        }

        let src = curr.get_src() as usize;
        let tgt = curr.get_tgt() as usize;
        let old_cmh = self.counter_moves[prev_p][prev_tgt][src][tgt];

        self.counter_moves[prev_p][prev_tgt][src][tgt] = Self::tapered_history(bonus, old_cmh);
    }

    fn update_followup(&mut self, curr: Move, prev: Move, bonus: i32, searched: &Vec<Move>) {
        let prev_p = prev.get_piece() as usize;
        let prev_tgt = prev.get_tgt() as usize;

        for m in searched {
            let src = m.get_src() as usize;
            let tgt = m.get_tgt() as usize;
            let old_fuh = self.followup_moves[prev_p][prev_tgt][src][tgt];

            self.followup_moves[prev_p][prev_tgt][src][tgt] =
                Self::tapered_history(-bonus, old_fuh);
        }

        let src = curr.get_src() as usize;
        let tgt = curr.get_tgt() as usize;
        let old_fuh = self.followup_moves[prev_p][prev_tgt][src][tgt];

        self.followup_moves[prev_p][prev_tgt][src][tgt] = Self::tapered_history(bonus, old_fuh);
    }

    /// Score individual captures.
    fn score_capture(&self, m: Move) -> i32 {
        if m.is_enpassant() {
            EP_SCORE
        } else {
            let mut score = MVV_LVA[m.get_piece() as usize][m.get_capture() as usize];

            if self.see(m) >= 0 {
                score += GOOD_CAPTURE_OFFSET
            }

            score
        }
    }

    /// Score assuming all moves are captures
    fn score_captures(&self, move_list: &mut MoveList) {
        for i in 0..move_list.len() {
            let m = move_list.moves[i];

            move_list.scores[i] = if m.is_promotion() {
                PROMOTION_SCORES[m.get_promotion() as usize]
            } else {
                self.score_capture(m)
            };
        }
    }

    /// Score quiet moves according to Standard/Counter Move/Follow Up heuristics
    fn score_history(&self, m: Move) -> i32 {
        let col = self.board.side as usize;
        let src = m.get_src() as usize;
        let tgt = m.get_tgt() as usize;

        let mut score = self.history_moves[col][src][tgt];

        // counter move
        if self.ply_from_null > 0 {
            let prev_move = self.history.last().unwrap().1;
            let prev_p = prev_move.get_piece() as usize;
            let prev_tgt = prev_move.get_tgt() as usize;

            score += self.counter_moves[prev_p][prev_tgt][src][tgt];

            // followup move
            if self.ply_from_null > 1 {
                let prev_move = self.history.get(self.history.len() - 2).unwrap().1; // impossible panic
                let prev_p = prev_move.get_piece() as usize;
                let prev_tgt = prev_move.get_tgt() as usize;

                score += self.followup_moves[prev_p][prev_tgt][src][tgt];
            }
        }

        HISTORY_OFFSET + score
    }

    /// Score any type of move
    fn score_move(&self, m: Move) -> i32 {
        if m.is_promotion() {
            PROMOTION_SCORES[m.get_promotion() as usize]
        } else if m.is_capture() {
            self.score_capture(m)
        } else {
            if m.is_castle() {
                CASTLE_SCORE
            } else if m == self.killer_moves[self.ply][0] {
                FIRST_KILLER_SCORE
            } else if m == self.killer_moves[self.ply][1] {
                SECOND_KILLER_SCORE
            } else {
                self.score_history(m)
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
    fn see(&self, m: Move) -> i32 {
        let mut swap_list: [i32; 32] = [0; 32];
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

    #[test]
    fn test_sorter() {
        init_all_tables();
        let pos: Position =
            "fen rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"
                .parse()
                .unwrap();
        let move_list = pos.generate_moves();

        let mut prev = -WORST;
        for (_, score) in move_list {
            assert!(prev >= score);
            prev = score;
        }
    }
}
