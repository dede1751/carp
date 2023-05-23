use std::time::Instant;
use std::{fmt, str::FromStr};

use crate::chess::{
    bitboard::*, castle::*, move_list::*, moves::*, piece::*, square::*, tables::*, zobrist::*,
};

use crate::engine::{nnue::*, search_params::*};

/// Piece-centric board representation
/// Any board without a king for each player (and with more than one for either) is UB!
#[derive(Copy, Clone, Debug)]
pub struct Board {
    pub pieces: [BitBoard; PIECE_COUNT],
    pub side_occupancy: [BitBoard; 2],
    pub occupancy: BitBoard,
    pub side: Color,
    pub castling_rights: CastlingRights,
    pub en_passant: Option<Square>,
    pub halfmoves: usize,
    pub hash: ZHash,

    // Various information used for evaluation and move generation
    diag_pins: BitBoard,
    hv_pins: BitBoard,
    pinned: BitBoard,
    block_check: BitBoard,
    capture_check: BitBoard,
    pub checkers: BitBoard,
    threats: BitBoard,
}

/// Pretty print board state
impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board_str = String::from("\n Board:\n\n\t┏━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┓");

        for rank in ALL_RANKS {
            board_str.push_str(format!("\n      {} ┃ ", 8 - rank as usize).as_str());

            for file in ALL_FILES {
                let square = Square::from_coords(file, rank);

                let piece_str = ALL_PIECES
                    .iter()
                    .find(|&p| self.pieces[*p as usize].get_bit(square))
                    .map_or(String::from(" "), |&p| p.to_string());

                board_str.push_str(&piece_str);
                board_str.push_str(" ┃ ");
            }
            if rank != Rank::First {
                board_str.push_str("\n\t┣━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━┫");
            }
        }
        board_str
            .push_str("\n\t┗━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┛\n\t  A   B   C   D   E   F   G   H\n");

        let en_passant_str = match self.en_passant {
            Some(square) => format!("{square}"),
            None => String::from("-"),
        };

        write!(
            f,
            "{board_str}
 Side to move      : {}
 Castling Rights   : {}
 En Passant Square : {en_passant_str}
 Halfmoves         : {}
 ",
            self.side, self.castling_rights, self.halfmoves,
        )
    }
}

/// Init board state from FEN string with complete error handling (no legality check)
impl FromStr for Board {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fen: Vec<&str> = s.split_whitespace().take(6).collect();
        if fen.len() != 6 {
            return Err("Invalid fen!");
        }

        let mut board = Board::new();
        let board_str = fen[0];
        let mut token_count = 0; // used for checking that number of tokens is correct

        let (mut file, mut rank) = (File::A, Rank::Eight);
        for token in board_str.chars() {
            match token {
                '/' => {
                    if token_count != 8 {
                        return Err("Invalid fen!");
                    };

                    rank = rank.down();
                    token_count = 0;
                }
                '1'..='8' => {
                    for _ in '1'..=token {
                        file = file.right();
                        token_count += 1;
                    }
                }
                _ => {
                    board.modify_piece::<true>(
                        Piece::try_from(token)?,
                        Square::from_coords(file, rank),
                    );
                    file = file.right();
                    token_count += 1;
                }
            }
        }

        if token_count != 8 {
            return Err("Invalid fen!");
        }

        match fen[1] {
            "w" => {
                board.side = Color::White;
                board.hash.toggle_side();
            }
            "b" => board.side = Color::Black,
            _ => return Err("Invalid fen!"),
        }

        let rights: CastlingRights = fen[2].parse()?;
        board.castling_rights = rights;
        board.hash.toggle_castle(rights);

        match fen[3] {
            "-" => board.en_passant = None,
            _ => {
                let ep_square: Square = fen[3].parse()?;

                board.en_passant = Some(ep_square);
                board.hash.toggle_ep(ep_square);
            }
        }

        match fen[4].parse::<usize>() {
            Ok(hm) => board.halfmoves = hm,
            Err(_) => return Err("Invalid halfmove count!"),
        }

        board.map_pins();
        board.map_checkers();
        board.map_king_threats();

        Ok(board)
    }
}

/// Convert board state to FEN string
impl Board {
    pub fn to_fen(&self) -> String {
        let mut fen = String::new();

        for rank in ALL_RANKS {
            let mut empty = 0;

            for file in ALL_FILES {
                let square = Square::from_coords(file, rank);

                let piece = ALL_PIECES
                    .iter()
                    .find(|&p| self.pieces[*p as usize].get_bit(square));

                if let Some(p) = piece {
                    if empty > 0 {
                        fen.push_str(&empty.to_string());
                        empty = 0;
                    }
                    fen.push(p.to_char())
                } else {
                    empty += 1;
                }
            }

            if empty > 0 {
                fen.push_str(&empty.to_string());
            }
            if rank != Rank::First {
                fen.push('/');
            }
        }

        match self.side {
            Color::White => fen.push_str(" w "),
            Color::Black => fen.push_str(" b "),
        }

        fen.push_str(&self.castling_rights.to_string());

        if let Some(square) = self.en_passant {
            fen.push_str(&format!(" {square}"));
        } else {
            fen.push_str(" -");
        }

        fen.push_str(&format!(" {} 1", self.halfmoves));

        fen
    }
}

/// Default to starting position
impl Default for Board {
    fn default() -> Self {
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            .parse()
            .unwrap()
    }
}

/// Implement all piece bitboard lookups (ugly because concatenating idents seems impossible)
macro_rules! impl_piece_lookups {
    ($($piece:ident, $own:ident, $opp:ident, $tot:ident),*) => {
        $(impl Board {
            pub const fn $own(&self) -> BitBoard {
                self.pieces[self.side.$piece() as usize]
            }

            pub const fn $opp(&self) -> BitBoard {
                self.pieces[self.side.$piece().opposite_color() as usize]
            }

            pub const fn $tot(&self) -> BitBoard {
                BitBoard(
                    self.pieces[Color::White.$piece() as usize].0 |
                    self.pieces[Color::Black.$piece() as usize].0
                )
            }
        })*
    };
}
impl_piece_lookups! {
    pawn, own_pawns, opp_pawns, pawns,
    knight, own_knights, opp_knights, knights,
    bishop, own_bishops, opp_bishops, bishops,
    rook, own_rooks, opp_rooks, rooks,
    queen, own_queens, opp_queens, queens,
    king, own_king, opp_king, kings
}

/// Implement side occupancy and diagonal/hv slider lookups
impl Board {
    pub const fn own_occupancy(&self) -> BitBoard {
        self.side_occupancy[self.side as usize]
    }
    pub const fn opp_occupancy(&self) -> BitBoard {
        self.side_occupancy[self.side as usize ^ 1]
    }
    pub const fn opp_queen_bishop(&self) -> BitBoard {
        BitBoard(self.opp_queens().0 | self.opp_bishops().0)
    }
    pub const fn opp_queen_rook(&self) -> BitBoard {
        BitBoard(self.opp_queens().0 | self.opp_rooks().0)
    }
}

const SET: bool = true;
const REMOVE: bool = false;

/// Implement board modification
impl Board {
    pub const fn new() -> Board {
        Board {
            pieces: [EMPTY_BB; PIECE_COUNT],
            side_occupancy: [EMPTY_BB; 2],
            occupancy: EMPTY_BB,
            side: Color::White,
            castling_rights: NO_RIGHTS,
            en_passant: None,
            halfmoves: 0,
            hash: NULL_HASH,

            diag_pins: EMPTY_BB,
            hv_pins: EMPTY_BB,
            pinned: EMPTY_BB,
            block_check: FULL_BB,
            capture_check: FULL_BB,
            checkers: EMPTY_BB,
            threats: EMPTY_BB,
        }
    }

    /// Set/remove piece while managing occupancy boards (remove first, set later)
    fn modify_piece<const SET: bool>(&mut self, piece: Piece, square: Square) {
        let p = piece as usize;
        let c = piece.color() as usize;

        if SET {
            self.pieces[p] = self.pieces[p].set_bit(square);
            self.occupancy = self.occupancy.set_bit(square);
            self.side_occupancy[c] = self.side_occupancy[c].set_bit(square);
        } else {
            self.pieces[p] = self.pieces[piece as usize].pop_bit(square);
            self.occupancy = self.occupancy.pop_bit(square);
            self.side_occupancy[c] = self.side_occupancy[c].pop_bit(square);
        }
        self.hash.toggle_piece(piece, square);
    }

    /// Makes (legal) move on the board
    /// Supplying illegal moves will lead to illegal board states.
    pub fn make_move(&self, m: Move) -> Board {
        let mut new = Board::new();

        // only clone the relevant info
        new.pieces = self.pieces;
        new.side_occupancy = self.side_occupancy;
        new.occupancy = self.occupancy;
        new.hash = self.hash;

        let (src, tgt) = (m.get_src(), m.get_tgt());
        let piece = m.get_piece();
        let promotion = m.get_promotion();

        new.halfmoves = self.halfmoves + 1;

        new.modify_piece::<REMOVE>(piece, src);
        if m.is_capture() || piece == Piece::WP || piece == Piece::BP {
            new.halfmoves = 0
        }

        if m.is_enpassant() {
            let ep_target = PUSH[!self.side as usize][tgt as usize];

            new.modify_piece::<REMOVE>(m.get_capture(), ep_target);
        } else if m.is_capture() {
            new.modify_piece::<REMOVE>(m.get_capture(), tgt);
        } else if m.is_castle() {
            let rook = self.side.rook();
            let (rook_src, rook_tgt) = ROOK_CASTLING_MOVE[tgt as usize];

            new.modify_piece::<REMOVE>(rook, rook_src);
            new.modify_piece::<SET>(rook, rook_tgt);
        }

        if m.is_promotion() {
            new.modify_piece::<SET>(promotion, tgt);
        } else {
            new.modify_piece::<SET>(piece, tgt);
        }

        if let Some(square) = self.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        if m.is_double_push() {
            let ep_tgt = PUSH[self.side as usize][src as usize];

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        let new_rights = self.castling_rights.update(src, tgt);

        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        new.side = !self.side;
        new.hash.toggle_side();

        // only once board is built, generate movegen info
        new.map_pins();
        new.map_checkers();
        new.map_king_threats();

        new
    }

    /// Make move with NNUE accumulator increments
    pub fn make_move_nnue(&self, m: Move, nnue_state: &mut Box<NNUEState>) -> Board {
        let mut new = Board::new();

        new.pieces = self.pieces;
        new.side_occupancy = self.side_occupancy;
        new.occupancy = self.occupancy;
        new.hash = self.hash;

        // add new accumulator
        nnue_state.push();

        let (src, tgt) = (m.get_src(), m.get_tgt());
        let piece = m.get_piece();
        let promotion = m.get_promotion();

        new.halfmoves = self.halfmoves + 1;

        new.modify_piece::<REMOVE>(piece, src);
        if m.is_capture() || piece == Piece::WP || piece == Piece::BP {
            new.halfmoves = 0
        }

        if m.is_enpassant() {
            let ep_target = PUSH[!self.side as usize][tgt as usize];
            let capture = m.get_capture();

            new.modify_piece::<REMOVE>(capture, ep_target);
            nnue_state.manual_update::<OFF>(capture, ep_target);
        } else if m.is_capture() {
            let capture = m.get_capture();

            new.modify_piece::<REMOVE>(capture, tgt);
            nnue_state.manual_update::<OFF>(capture, tgt);
        } else if m.is_castle() {
            let rook = self.side.rook();
            let (rook_src, rook_tgt) = ROOK_CASTLING_MOVE[tgt as usize];

            new.modify_piece::<REMOVE>(rook, rook_src);
            new.modify_piece::<SET>(rook, rook_tgt);
            nnue_state.move_update(rook, rook_src, rook_tgt);
        }

        if m.is_promotion() {
            new.modify_piece::<SET>(promotion, tgt);
            nnue_state.manual_update::<OFF>(piece, src);
            nnue_state.manual_update::<ON>(promotion, tgt);
        } else {
            new.modify_piece::<SET>(piece, tgt);
            nnue_state.move_update(piece, src, tgt);
        }

        if let Some(square) = self.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        if m.is_double_push() {
            let ep_tgt = PUSH[self.side as usize][src as usize];

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        let new_rights = self.castling_rights.update(src, tgt);

        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        new.side = !self.side;
        new.hash.toggle_side();

        new.map_pins();
        new.map_checkers();
        new.map_king_threats();

        new
    }

    /// Makes the null move on the board, giving the turn to the opponent
    pub fn make_null(&self) -> Board {
        let mut new = Board::new();

        new.pieces = self.pieces;
        new.side_occupancy = self.side_occupancy;
        new.occupancy = self.occupancy;
        new.hash = self.hash;

        new.side = !self.side;
        new.hash.toggle_side();

        new.en_passant = None;
        if let Some(square) = self.en_passant {
            new.hash.toggle_ep(square);
        }

        new.map_pins();
        new.map_checkers();
        new.map_king_threats();

        new
    }

    /// Set attackers to all enemy pieces directly attacking the king.
    /// If there is at least one attacker, initialize the bitboards for blocking/capturing the check
    fn map_checkers(&mut self) {
        let square = self.own_king().lsb();

        self.checkers = self.opp_pawns() & pawn_attacks(square, self.side)   | // pawns
            self.opp_knights() & knight_attacks(square)                      | // knights
            self.opp_queen_bishop() & bishop_attacks(square, self.occupancy) | // bishops + queens
            self.opp_queen_rook()   & rook_attacks(square, self.occupancy)   | // rooks + queens
            self.opp_king() & king_attacks(square); // kings

        if self.checkers.count_bits() == 1 {
            self.block_check = BETWEEN[square as usize][self.checkers.lsb() as usize];
            self.capture_check = self.checkers;
        }
    }

    /// Sets threats to all attacked squares by the opponent to see where the king can move
    ///
    /// We pretend the king is not on the board so that sliders also attack behind the king, since
    /// otherwise that square would be considered not attacked
    fn map_king_threats(&mut self) {
        let king_square = self.own_king().lsb();
        let occupancies = self.occupancy.pop_bit(king_square);

        self.threats = self
            .opp_pawns()
            .into_iter()
            .map(|sq| pawn_attacks(sq, !self.side))
            .fold(EMPTY_BB, |acc, x| acc | x)
            | self
                .opp_knights()
                .into_iter()
                .map(knight_attacks)
                .fold(EMPTY_BB, |acc, x| acc | x)
            | self
                .opp_queen_bishop()
                .into_iter()
                .map(|sq| bishop_attacks(sq, occupancies))
                .fold(EMPTY_BB, |acc, x| acc | x)
            | self
                .opp_queen_rook()
                .into_iter()
                .map(|sq| rook_attacks(sq, occupancies))
                .fold(EMPTY_BB, |acc, x| acc | x)
            | self
                .opp_king()
                .into_iter()
                .map(king_attacks)
                .fold(EMPTY_BB, |acc, x| acc | x);
    }

    /// Generates pinned pieces and diagonal/orthogonal pin maps
    ///
    /// Pin masks are defined as the squares between a pinning enemy piece and one's own king.
    /// Any pinned piece can safely move along these squares (simply & moves with pinmask).
    /// For simplicity, pin masks also indirectly include the check mask (this has no actual
    /// effect on the pin use, as no piece can be sitting on the check mask anyways)
    fn map_pins(&mut self) {
        let king_square = self.own_king().lsb();

        // get all own pieces on diagonal/orthogonal rays from the king
        let possible_diag_pins = bishop_attacks(king_square, self.occupancy) & self.own_occupancy();
        let possible_hv_pins = rook_attacks(king_square, self.occupancy) & self.own_occupancy();

        // remove the possible pinned pieces
        let remove_diag_blockers = self.occupancy & !possible_diag_pins;
        let remove_hv_blockers = self.occupancy & !possible_hv_pins;

        // get all pinning pieces (pieces that see the king with pinned pieces removed)
        let diag_attackers =
            bishop_attacks(king_square, remove_diag_blockers) & self.opp_queen_bishop();
        let hv_attackers = rook_attacks(king_square, remove_hv_blockers) & self.opp_queen_rook();

        // pin masks are between the attacker and the king square (attacker included)
        self.diag_pins = diag_attackers
            .into_iter()
            .map(|sq| BETWEEN[sq as usize][king_square as usize].set_bit(sq))
            .fold(EMPTY_BB, |acc, x| acc | x);

        self.hv_pins = hv_attackers
            .into_iter()
            .map(|sq| BETWEEN[sq as usize][king_square as usize].set_bit(sq))
            .fold(EMPTY_BB, |acc, x| acc | x);

        // pinned pieces are own pieces along any pin mask
        self.pinned = (self.diag_pins | self.hv_pins) & self.own_occupancy();
    }
}

const N: usize = 2;
const B: usize = 4;
const R: usize = 6;
const Q: usize = 8;
pub const QUIETS: bool = true;
pub const CAPTURES: bool = false;

/// Implement board move generation
impl Board {
    /// Looks for which piece was captured on tgt square
    /// Panics if no piece is set on the tgt square. Only call if it's sure to be a capture.
    fn get_captured_piece(&self, tgt: Square) -> Piece {
        (PIECES[!self.side as usize])
            .into_iter()
            .find(|&p| self.pieces[p as usize].get_bit(tgt))
            .unwrap() // possible panic
    }

    /// Given a target map for the source square, it adds all possible moves to the move list
    /// If QUIET==false, only captures are added.
    fn insert_moves<const QUIET: bool>(
        &self,
        piece: Piece,
        source: Square,
        targets: BitBoard,
        move_list: &mut MoveList,
    ) {
        let captures = targets & self.opp_occupancy();
        for target in captures {
            move_list.add_capture(source, target, piece, self.get_captured_piece(target));
        }

        if QUIET {
            let quiets = targets & !self.occupancy;
            for target in quiets {
                move_list.add_quiet(source, target, piece, 0);
            }
        }
    }

    /// Generate all legal king moves, or only captures if QUIET==false
    fn gen_king_moves<const QUIET: bool>(&self, move_list: &mut MoveList) {
        let king_square = self.own_king().lsb();
        let targets = king_attacks(king_square) & !self.threats;

        self.insert_moves::<QUIET>(self.side.king(), king_square, targets, move_list);
    }

    /// Generate all legal pawn quiet moves
    fn gen_pawn_quiets(&self, move_list: &mut MoveList) {
        const START_RANKS: [Rank; 2] = [Rank::Second, Rank::Seventh];

        let side = self.side as usize;
        let pawn_bb = self.own_pawns() & !self.diag_pins; // diag pinned pawns cannot move

        for source in pawn_bb {
            let target: Square = PUSH[side][source as usize];

            // pawns pinned along a rank cannot be pushed
            if self.hv_pins.get_bit(source) && !self.hv_pins.get_bit(target) {
                continue;
            }

            if !(self.occupancy.get_bit(target)) {
                // normal pawn push
                if self.block_check.get_bit(target) {
                    move_list.add_pawn_quiet(source, target, self.side, 0);
                }

                // double pawn push
                if source.rank() == START_RANKS[side] {
                    let target = DOUBLE_PUSH[side][source.file() as usize];

                    if !(self.occupancy.get_bit(target)) && self.block_check.get_bit(target) {
                        move_list.add_pawn_quiet(source, target, self.side, 1);
                    }
                }
            }
        }
    }

    /// Generate all legal pawn captures (including enpassant)
    fn gen_pawn_captures(&self, move_list: &mut MoveList) {
        let pawn_bb = self.own_pawns() & !self.hv_pins; // hv pinned pawns cannot capture
        let check_mask = self.block_check | self.capture_check;

        for source in pawn_bb {
            let mut attacks: BitBoard = pawn_attacks(source, self.side);

            // if pinned, only capture along diag pin ray (also goes for enpassant)
            if self.diag_pins.get_bit(source) {
                attacks &= self.diag_pins
            }

            let captures: BitBoard = attacks & check_mask & self.opp_occupancy();

            // normal/enpassant capture
            for target in captures {
                let captured_piece = self.get_captured_piece(target);

                move_list.add_pawn_capture(source, target, self.side, captured_piece);
            }

            if let Some(ep_square) = self.en_passant {
                let ep_target = PUSH[!self.side as usize][ep_square as usize];

                // Attack must land on ep square, and either capture the checking piece or
                // block the check
                if attacks.get_bit(ep_square)
                    && (self.capture_check.get_bit(ep_target)
                        || self.block_check.get_bit(ep_square))
                {
                    let ep_rank = RANK_MASKS[ep_target as usize];

                    // En Passant discovered check!
                    if ep_rank & self.own_king() != EMPTY_BB
                        && ep_rank & self.opp_queen_rook() != EMPTY_BB
                    {
                        // remove the two pawns
                        let occupancy = self.occupancy & !source.to_board() & !ep_target.to_board();

                        let king_square = self.own_king().lsb();
                        let king_ray = rook_attacks(king_square, occupancy) & ep_rank;

                        // king sees enemy queen or rook directly
                        if king_ray & self.opp_queen_rook() != EMPTY_BB {
                            continue;
                        }
                    }

                    move_list.add_enpassant(source, ep_square, self.side);
                }
            }
        }
    }

    /// Generate all legal castling moves
    fn gen_castling_moves(&self, move_list: &mut MoveList) {
        const SRC: [Square; 2] = [Square::E1, Square::E8];
        const K_TGT: [Square; 2] = [Square::G1, Square::G8];
        const Q_TGT: [Square; 2] = [Square::C1, Square::C8];
        const K_OCCS: [BitBoard; 2] = [BitBoard(6917529027641081856), BitBoard(96)];
        const Q_OCCS: [BitBoard; 2] = [BitBoard(1008806316530991104), BitBoard(14)];
        const Q_THREATS: [BitBoard; 2] = [BitBoard(864691128455135232), BitBoard(12)];

        let side = self.side as usize;
        let piece = self.side.king();

        if self.castling_rights.has_kingside(self.side)
            && (self.threats | self.occupancy) & K_OCCS[side] == EMPTY_BB
        {
            move_list.add_quiet(SRC[side], K_TGT[side], piece, 1);
        }

        if self.castling_rights.has_queenside(self.side)
            && self.occupancy & Q_OCCS[side] == EMPTY_BB
            && self.threats & Q_THREATS[side] == EMPTY_BB
        {
            move_list.add_quiet(SRC[side], Q_TGT[side], piece, 1);
        }
    }

    /// Generate all legal moves for any standard piece.
    /// PIECE is crate::Piece as usize
    /// if QUIET==false, only generate captures
    fn gen_piece_moves<const PIECE: usize, const QUIET: bool>(&self, move_list: &mut MoveList) {
        let side = self.side as usize;
        let piece = Piece::from(PIECE + side);
        let mut piece_bb = self.pieces[PIECE + side];
        let check_mask = self.block_check | self.capture_check;

        if PIECE == N {
            piece_bb &= !self.pinned; // pinned knights cannot move
        } else if PIECE == B {
            piece_bb &= !self.hv_pins; // hv-pinned bishops cannot move
        } else if PIECE == R {
            piece_bb &= !self.diag_pins // diag-pinned rooks cannot move
        }

        for source in piece_bb {
            let mut targets = EMPTY_BB;

            if PIECE == N {
                targets = knight_attacks(source);
            } else if PIECE == B {
                targets = bishop_attacks(source, self.occupancy);

                if self.diag_pins.get_bit(source) {
                    targets &= self.diag_pins // move along diagonal pin ray
                }
            } else if PIECE == R {
                targets = rook_attacks(source, self.occupancy);

                if self.hv_pins.get_bit(source) {
                    targets &= self.hv_pins // move along orthogonal pin ray
                }
            } else if PIECE == Q {
                // queen, when pinned, behaves like a rook or a bishop
                if self.diag_pins.get_bit(source) {
                    targets = bishop_attacks(source, self.occupancy) & self.diag_pins;
                } else if self.hv_pins.get_bit(source) {
                    targets = rook_attacks(source, self.occupancy) & self.hv_pins;
                } else {
                    targets = queen_attacks(source, self.occupancy);
                }
            }
            targets &= check_mask;

            self.insert_moves::<QUIET>(piece, source, targets, move_list);
        }
    }

    /// Generate all legal moves, or only captures if QUIET==false
    pub fn gen_moves<const QUIET: bool>(&self) -> MoveList {
        let mut move_list: MoveList = MoveList::default();
        let attacker_count = self.checkers.count_bits();

        self.gen_king_moves::<QUIET>(&mut move_list);

        // with double checks, only king moves are legal, so we stop here
        if attacker_count > 1 {
            return move_list;
        }

        if QUIET && self.castling_rights != NO_RIGHTS && attacker_count == 0 {
            self.gen_castling_moves(&mut move_list);
        }

        // generate all the legal piece moves using pin and blocker/capture masks
        self.gen_pawn_captures(&mut move_list);
        if QUIET {
            self.gen_pawn_quiets(&mut move_list);
        }

        self.gen_piece_moves::<N, QUIET>(&mut move_list);
        self.gen_piece_moves::<B, QUIET>(&mut move_list);
        self.gen_piece_moves::<R, QUIET>(&mut move_list);
        self.gen_piece_moves::<Q, QUIET>(&mut move_list);

        move_list
    }

    /// Finds legal move in board from the uci-formatted move string
    pub fn find_move(&self, move_str: &str) -> Option<Move> {
        self.gen_moves::<QUIETS>()
            .moves
            .into_iter()
            .find(|m| m.to_string() == move_str)
    }
}

/// SEE
impl Board {
    /// Returns bitboard with all pieces attacking a square
    fn map_all_attackers(&self, square: Square, blockers: BitBoard) -> BitBoard {
        self.pieces[WPAWN] & pawn_attacks(square, Color::Black)
            | self.pieces[BPAWN] & pawn_attacks(square, Color::White)
            | self.knights() & knight_attacks(square)
            | (self.bishops() | self.queens()) & bishop_attacks(square, blockers)
            | (self.rooks() | self.queens()) & rook_attacks(square, blockers)
            | self.kings() & king_attacks(square)
    }

    /// Returns the least valuable of the attackers within the attacker map
    fn get_lva(&self, attackers: BitBoard, side: Color) -> Option<(Square, Piece)> {
        for piece in PIECES[side as usize] {
            let squares = attackers & self.pieces[piece as usize];

            if squares != EMPTY_BB {
                return Some((squares.lsb(), piece));
            }
        }

        None
    }

    /// Checks if the static exchange after a move is enough to beat the given threshold
    /// This can be used for both captures and quiet moves.
    /// This implementation is basically that seen in Viri, which in turn is that of Ethereal
    pub fn see(&self, m: Move, threshold: Eval) -> bool {
        let src = m.get_src();
        let tgt = m.get_tgt();

        // Piece being swapped off is the promoted piece
        let victim = if m.is_promotion() {
            m.get_promotion()
        } else {
            m.get_piece()
        };

        // Get the static move value (also works for quiets)
        let mut move_value = if m.is_capture() {
            PIECE_VALUES[m.get_capture() as usize]
        } else {
            0
        };
        if m.is_promotion() {
            move_value += PIECE_VALUES[victim as usize] - PIECE_VALUES[0];
        }

        // Lose if the balance is already in our opponent's favor and it's their turn
        let mut balance = move_value - threshold;
        if balance < 0 {
            return false;
        }

        // Win if the balance is still in our favor even if we lose the capturing piece
        balance -= PIECE_VALUES[victim as usize];
        if balance > 0 {
            return true;
        }

        let diagonal_sliders = self.bishops() | self.queens();
        let orthogonal_sliders = self.rooks() | self.queens();

        // Updated occupancy map after capture
        let mut occs = self.occupancy.pop_bit(src).set_bit(tgt);
        if m.is_enpassant() {
            let ep_tgt = PUSH[!self.side as usize][self.en_passant.unwrap() as usize]; // guaranteed to be Some
            occs = occs.pop_bit(ep_tgt); 
        }

        // Get all pieces covering the exchange square and start exchanging
        let mut attackers = self.map_all_attackers(tgt, occs) & occs;
        let mut side_to_move = !self.side;

        loop {
            // SEE terminates when no recapture is possible.
            let own_attackers = attackers & self.side_occupancy[side_to_move as usize];
            if own_attackers == EMPTY_BB {
                break;
            }

            // Get the least valuable attacker and simulate the recapture
            let (attacker_square, attacker) = self.get_lva(own_attackers, side_to_move).unwrap(); // attackers are at least one
            occs = occs.pop_bit(attacker_square);

            // Diagonal recaptures uncover bishops/queens
            if attacker == side_to_move.pawn()
                || attacker == side_to_move.bishop()
                || attacker == side_to_move.queen()
            {
                attackers |= bishop_attacks(tgt, occs) & diagonal_sliders;
            }

            // Orthogonal recaptures uncover rooks/queens
            if attacker == side_to_move.rook() || attacker == side_to_move.queen() {
                attackers |= rook_attacks(tgt, occs) & orthogonal_sliders;
            }
            attackers &= occs; // ignore pieces already "used up"

            // Negamax the balance, cutoff if losing our attacker would still win the exchange
            side_to_move = !side_to_move;
            balance = -balance - 1 - PIECE_VALUES[attacker as usize];
            if balance >= 0 {
                // If the recapturing piece is a king, and the opponent has another attacker,
                // a positive balance should not translate to an exchange win.
                if attacker == (!side_to_move).king()
                    && attackers & self.side_occupancy[side_to_move as usize] != EMPTY_BB
                {
                    return self.side == side_to_move;
                }

                break;
            }
        }

        // We win the exchange if we are not the one who should recapture
        self.side != side_to_move
    }
}

/// Perft
impl Board {
    /// Recursive move generation
    fn perft_driver(&self, depth: usize) -> u64 {
        let move_list = self.gen_moves::<QUIETS>();

        if depth == 1 {
            return move_list.len() as u64;
        } else if depth == 0 {
            return 1;
        }

        let mut nodes = 0;
        for i in 0..move_list.len() {
            let m = move_list.moves[i];
            let new_board = self.make_move(m);
            nodes += new_board.perft_driver(depth - 1);
        }

        nodes
    }

    /// Cumulative (divide) perft
    pub fn perft(&self, depth: usize) -> u64 {
        let move_list = self.gen_moves::<QUIETS>();
        let mut total_nodes = 0;

        let start = Instant::now();
        for i in 0..move_list.len() {
            let m = move_list.moves[i];
            let start = Instant::now();
            let root = self.make_move(m);
            let nodes = root.perft_driver(depth - 1);
            total_nodes += nodes;
            let duration = start.elapsed();

            println!(
                "{}{} -- {} nodes in {:?}",
                m.get_src(),
                m.get_tgt(),
                nodes,
                duration
            );
        }
        let duration = start.elapsed();

        let perf: u128 = total_nodes as u128 / duration.as_micros();
        println!("\n{total_nodes} nodes in {duration:?} - {perf}Mnodes/s");

        total_nodes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invalid_fen() {
        let invalid_pieces =
            "rnbqkbnr/pp2ppppp/7/2p5/4P3/5N2/PPPP1P/RNBQKB1R b - - 1 2".parse::<Board>();
        let invalid_side =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1".parse::<Board>();
        let invalid_castle =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w kqKQ - 0 1".parse::<Board>();
        let invalid_ep_square =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ a9 0 1".parse::<Board>();

        assert!(invalid_pieces.is_err());
        assert!(invalid_side.is_err());
        assert!(invalid_castle.is_err());
        assert!(invalid_ep_square.is_err());
    }

    #[test]
    fn test_pin_mask() {
        init_all_tables();
        let board: Board = "R2bk3/5p2/4r1B1/1Q6/8/4Q3/4R3/2K5 b - - 0 1"
            .parse()
            .unwrap();
        println!("{board}");

        println!("{}\n{}\n{}", board.pinned, board.diag_pins, board.hv_pins);

        assert!(board.pinned.get_bit(Square::F7));
        assert!(board.pinned.get_bit(Square::E6));
        assert!(board.diag_pins.get_bit(Square::G6));
        assert!(board.hv_pins.get_bit(Square::C8));
        assert!(board.hv_pins.get_bit(Square::E3));
        assert!(!board.hv_pins.get_bit(Square::E2));
    }

    #[test]
    fn test_legal_pawn() {
        init_all_tables();
        let b1: Board = "8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3 0 1".parse().unwrap();
        println!("{b1}");
        let m1 = b1.gen_moves::<QUIETS>(); // enpassant blocks check
        assert_eq!(m1.len(), 6);

        let b2: Board = "8/8/8/2k5/3Pp3/8/8/4K3 b - d3 0 1".parse().unwrap();
        println!("{b2}");
        let m2 = b2.gen_moves::<QUIETS>(); // enpassant captures checker
        assert_eq!(m2.len(), 9);

        let b3: Board = "8/8/8/8/k2Pp2Q/8/8/3K4 b - d3 0 1".parse().unwrap();
        println!("{b3}");
        let m3 = b3.gen_moves::<QUIETS>(); // enpassant would leave the king in check
        assert_eq!(m3.len(), 6);
    }

    #[test]
    fn test_see_helpers() {
        init_all_tables();
        let b1: Board = "1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1"
            .parse()
            .unwrap();
        let b2: Board = "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1"
            .parse()
            .unwrap();

        let att1 = b1.map_all_attackers(Square::E5, b1.occupancy);
        let att2 = b2.map_all_attackers(Square::E5, b2.occupancy);

        println!("{b1}\n{att1}\n{b2}\n{att2}");

        assert!(att1.get_bit(Square::E1));
        assert!(!att1.get_bit(Square::D8));

        assert!(att2.get_bit(Square::E2));
        assert!(!att2.get_bit(Square::E1));
    }

    #[test]
    fn test_see() {
        const SEE_SUITE: [(&str, &str, bool); 4] = [
            (
                "1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1",
                "e1e5",
                true,
            ),
            (
                "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1",
                "d3e5",
                false,
            ),
            (
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
                "g2h3",
                true,
            ),
            ("k3r3/8/8/4p3/8/2B5/1B6/K7 w - - 0 1", "c3e5", true),
        ];

        init_all_tables();
        for (b, m, r) in SEE_SUITE {
            let board: Board = b.parse().unwrap();
            let mov: Move = board.find_move(m).unwrap();

            println!("{board}");
            assert_eq!(board.see(mov, 0), r);
        }
    }

    #[rustfmt::skip]
    const PERFT_SUITE: [(&str, &str, u64, usize); 14] = [
        ("8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1", "Illegal ep move #1", 1015133, 6),
        ("3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", "Illegal ep move #2", 1134888, 6),
        ("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", "Ep capture checks opponent", 1440467, 6),
        ("5k2/8/8/8/8/8/8/4K2R w K - 0 1", "Short castling gives check", 661072, 6),
        ("3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", "Long castling gives check", 803711, 6),
        ("r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", "Castle rights", 1274206, 4),
        ("r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", "Castling prevented", 1720476, 4),
        ("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", "Promote out of check", 3821001, 6),
        ("8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", "Discovered check", 1004658, 5),
        ("4k3/1P6/8/8/8/8/K7/8 w - - 0 1", "Promote to give check", 217342, 6),
        ("8/P1k5/K7/8/8/8/8/8 w - - 0 1", "Under promote to give check", 92683, 6),
        ("K1k5/8/P7/8/8/8/8/8 w - - 0 1", "Self stalemate", 2217, 6),
        ("8/k1P5/8/1K6/8/8/8/8 w - - 0 1", "Stalemate & checkmate #1", 567584, 7),
        ("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", "Stalemate & checkmate #2", 23527, 4)
    ];

    #[test]
    fn perft_default_6() {
        init_all_tables();
        let board = Board::default();
        let nodes = board.perft(6);

        assert_eq!(nodes, 119060324);
    }

    #[test]
    fn perft_kiwipete_5() {
        init_all_tables();
        let board: Board = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
            .parse()
            .unwrap();
        let nodes = board.perft(5);

        assert_eq!(nodes, 193690690);
    }

    #[test]
    fn perft_suite() {
        init_all_tables();
        for (fen, description, correct_count, depth) in PERFT_SUITE {
            let board: Board = fen.parse().unwrap();
            println!("{fen}\n{description}\n{board}");

            let nodes = board.perft(depth);
            assert_eq!(nodes, correct_count);
        }
    }
}
