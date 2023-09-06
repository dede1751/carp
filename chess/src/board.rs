use std::time::Instant;
use std::{fmt, str::FromStr};

use crate::{
    bitboard::BitBoard,
    castle::{rook_castling_move, CastlingRights},
    move_list::MoveList,
    movegen::*,
    moves::{Move, MoveType},
    nnue::{NNUEState, OFF, ON},
    params::*,
    piece::{Color, Piece},
    square::{File, Rank, Square},
    zobrist::ZHash,
};

/// Piece-centric board representation
/// Any board without a king for each player (and with more than one for either) is UB!
#[derive(Clone, Debug)]
pub struct Board {
    // Main bitboards
    piece_bb: [BitBoard; Piece::COUNT],
    side_bb: [BitBoard; 2],

    // Piece map for piece_at lookup
    piece: [Option<Piece>; Square::COUNT],

    // Other positional information
    pub side: Color,
    pub castling_rights: CastlingRights,
    pub en_passant: Option<Square>,
    pub halfmoves: usize,
    pub hash: ZHash,

    // Checkers kept for in_check() within search
    pub checkers: BitBoard,
}

/// Pretty print board state
impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board_str = format!(
            "\n FEN: {}\n\n\t┏━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┓",
            self.to_fen()
        );

        for rank in Rank::ALL {
            board_str.push_str(format!("\n      {} ┃ ", 8 - rank as usize).as_str());

            for file in File::ALL {
                let square = Square::from_coords(file, rank);
                let piece_str =
                    self.piece[square as usize].map_or(String::from(" "), |p| p.to_string());

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

        let mut board = Self::new();
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
                    board.set_piece(Piece::try_from(token)?, Square::from_coords(file, rank));
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

        board.checkers = board.checkers();

        Ok(board)
    }
}

/// Convert board state to FEN string
impl Board {
    pub fn to_fen(&self) -> String {
        let mut fen = String::new();

        for rank in Rank::ALL {
            let mut empty = 0;

            for file in File::ALL {
                let square = Square::from_coords(file, rank);

                if let Some(p) = self.piece[square as usize] {
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
    ($($piece:expr, $own:ident, $opp:ident, $tot:ident),*) => {
        $(impl Board {
            pub const fn $own(&self) -> BitBoard {
                BitBoard(self.piece_bb[$piece].0 & self.side_bb[self.side as usize].0)
            }
            pub const fn $opp(&self) -> BitBoard {
                BitBoard(self.piece_bb[$piece].0 & self.side_bb[self.side as usize ^ 1].0)
            }
            pub const fn $tot(&self) -> BitBoard {
                self.piece_bb[$piece]
            }
        })*
    };
}
impl_piece_lookups! {
    0, own_pawns, opp_pawns, pawns,
    1, own_knights, opp_knights, knights,
    2, own_bishops, opp_bishops, bishops,
    3, own_rooks, opp_rooks, rooks,
    4, own_queens, opp_queens, queens,
    5, own_king, opp_king, kings
}

/// Implement side occupancy and diagonal/hv slider lookups
impl Board {
    pub const fn white(&self) -> BitBoard {
        self.side_bb[Color::White as usize]
    }
    pub const fn black(&self) -> BitBoard {
        self.side_bb[Color::Black as usize]
    }
    pub const fn occupancy(&self) -> BitBoard {
        BitBoard(self.white().0 | self.black().0)
    }
    pub const fn piece_occupancy(&self, piece: Piece) -> BitBoard {
        BitBoard(self.piece_bb[piece.index()].0 & self.side_bb[piece.color() as usize].0)
    }
    pub const fn own_occupancy(&self) -> BitBoard {
        self.side_bb[self.side as usize]
    }
    pub const fn opp_occupancy(&self) -> BitBoard {
        self.side_bb[self.side as usize ^ 1]
    }
    pub const fn opp_queen_bishop(&self) -> BitBoard {
        BitBoard(self.opp_queens().0 | self.opp_bishops().0)
    }
    pub const fn opp_queen_rook(&self) -> BitBoard {
        BitBoard(self.opp_queens().0 | self.opp_rooks().0)
    }
}

/// Implement board modification
impl Board {
    /// Initialize an empty board
    pub const fn new() -> Self {
        Self {
            piece_bb: [BitBoard::EMPTY; Piece::COUNT],
            side_bb: [BitBoard::EMPTY; 2],

            piece: [None; Square::COUNT],

            side: Color::White,
            castling_rights: CastlingRights::NONE,
            en_passant: None,
            halfmoves: 0,
            hash: ZHash::NULL,

            checkers: BitBoard::EMPTY,
        }
    }

    /// Looks for which piece is on the given Square
    /// Panics if no piece is on that square
    pub fn piece_at(&self, square: Square) -> Piece {
        self.piece[square as usize].unwrap()
    }

    /// Return piece captured by a capture Move
    /// Panics if not called on a capture
    pub fn get_capture(&self, m: Move) -> Piece {
        if m.get_type() == MoveType::EnPassant {
            (!self.side).pawn()
        } else {
            self.piece_at(m.get_tgt())
        }
    }

    /// Mask all attackers of a certain square, given the blocker bitboard.
    fn attackers(&self, square: Square, blockers: BitBoard) -> BitBoard {
        self.opp_occupancy()
            & (self.knights() & knight_attacks(square)
                | self.kings() & king_attacks(square)
                | self.pawns() & pawn_attacks(square, self.side)
                | (self.queens() | self.rooks()) & rook_attacks(square, blockers)
                | (self.queens() | self.bishops()) & bishop_attacks(square, blockers))
    }

    /// Mask all checking pieces
    fn checkers(&self) -> BitBoard {
        self.attackers(self.own_king().lsb(), self.occupancy())
    }

    /// Returns true if the square is attacked by at least one enemy piece
    fn square_attacked(&self, square: Square, blockers: BitBoard) -> bool {
        self.attackers(square, blockers) != BitBoard::EMPTY
    }

    /// Set the piece on the board at the given square (remove first, set later)
    fn set_piece(&mut self, piece: Piece, square: Square) {
        let p = piece.index();
        let c = piece.color() as usize;

        self.piece_bb[p] = self.piece_bb[p].set_bit(square);
        self.side_bb[c] = self.side_bb[c].set_bit(square);
        self.piece[square as usize] = Some(piece);
        self.hash.toggle_piece(piece, square);
    }

    /// Remove the piece at the given square on the board (set first, remove later)
    /// The piece must exist at the given square
    fn remove_piece(&mut self, square: Square) {
        let piece = self.piece_at(square);
        let p = piece.index();
        let c = piece.color() as usize;

        self.piece_bb[p] = self.piece_bb[p].pop_bit(square);
        self.side_bb[c] = self.side_bb[c].pop_bit(square);
        self.piece[square as usize] = None;
        self.hash.toggle_piece(piece, square);
    }

    /// Makes (legal) move on the board
    /// Supplying illegal moves will lead to illegal board states.
    pub fn make_move(&self, m: Move) -> Board {
        let mut new = self.clone();
        let (src, tgt) = (m.get_src(), m.get_tgt());
        let piece = self.piece_at(src); // must exist
        let move_type = m.get_type();
        let capture = move_type.is_capture();

        // Remove moving piece and reset halfmoves
        new.remove_piece(src);
        if capture || piece.is_pawn() {
            new.halfmoves = 0
        } else {
            new.halfmoves += 1;
        }

        // Handle pieces affected by the move (captures/castles..)
        if move_type == MoveType::EnPassant {
            new.remove_piece(tgt.forward(!self.side));
        } else if capture {
            new.remove_piece(tgt);
        } else if move_type == MoveType::Castle {
            let rook = self.side.rook();
            let (rook_src, rook_tgt) = rook_castling_move(tgt);

            new.remove_piece(rook_src);
            new.set_piece(rook, rook_tgt);
        }

        // Move the piece to the new square
        if move_type.is_promotion() {
            new.set_piece(move_type.get_promotion(self.side), tgt);
        } else {
            new.set_piece(piece, tgt);
        }

        // Handle enpassant
        if let Some(square) = self.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        // Handle double push
        if move_type == MoveType::DoublePush {
            let ep_tgt = src.forward(self.side);

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        // Handle castling rights
        let new_rights = self.castling_rights.update(src, tgt);
        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        new.side = !self.side;
        new.hash.toggle_side();
        new.checkers = new.checkers();

        new
    }

    /// Make move with NNUE accumulator increments
    pub fn make_move_nnue(&self, m: Move, nnue_state: &mut Box<NNUEState>) -> Board {
        let mut new = self.clone();
        let (src, tgt) = (m.get_src(), m.get_tgt());
        let piece = self.piece_at(src);
        let move_type = m.get_type();
        let capture = move_type.is_capture();

        // add new accumulator
        nnue_state.push();

        new.remove_piece(src);
        if capture || piece.is_pawn() {
            new.halfmoves = 0
        } else {
            new.halfmoves += 1;
        }

        if move_type == MoveType::EnPassant {
            let ep_target = tgt.forward(!self.side);

            new.remove_piece(ep_target);
            nnue_state.manual_update::<OFF>((!self.side).pawn(), ep_target);
        } else if capture {
            new.remove_piece(tgt);
            nnue_state.manual_update::<OFF>(self.piece_at(tgt), tgt);
        } else if move_type == MoveType::Castle {
            let rook = self.side.rook();
            let (rook_src, rook_tgt) = rook_castling_move(tgt);

            new.remove_piece(rook_src);
            new.set_piece(rook, rook_tgt);
            nnue_state.move_update(rook, rook_src, rook_tgt);
        }

        if move_type.is_promotion() {
            let promotion = move_type.get_promotion(self.side);

            new.set_piece(promotion, tgt);
            nnue_state.manual_update::<OFF>(piece, src);
            nnue_state.manual_update::<ON>(promotion, tgt);
        } else {
            new.set_piece(piece, tgt);
            nnue_state.move_update(piece, src, tgt);
        }

        if let Some(square) = self.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        if move_type == MoveType::DoublePush {
            let ep_tgt = src.forward(self.side);

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        let new_rights = self.castling_rights.update(src, tgt);
        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        new.side = !self.side;
        new.hash.toggle_side();
        new.checkers = new.checkers();

        new
    }

    /// Makes the null move on the board, giving the turn to the opponent
    /// Calling this function when in check breaks the game state.
    pub fn make_null(&self) -> Board {
        let mut new = self.clone();
        new.side = !self.side;
        new.hash.toggle_side();

        new.en_passant = None;
        if let Some(square) = self.en_passant {
            new.hash.toggle_ep(square);
        }
        new.checkers = BitBoard::EMPTY;

        new
    }
}

pub const QUIETS: bool = true;
pub const CAPTURES: bool = false;
const KS: usize = 0;
const QS: usize = 1;

/// Implement board move generation
impl Board {
    /// Generate all legal king moves, or only captures if QUIET==false
    fn gen_king_moves<const QUIET: bool>(&self, move_list: &mut MoveList) {
        let src = self.own_king().lsb();
        let attacks = king_attacks(src);
        let blockers = self.occupancy().pop_bit(src);

        for tgt in attacks & self.opp_occupancy() {
            if !self.square_attacked(tgt, blockers) {
                move_list.push(Move::new(src, tgt, MoveType::Capture));
            }
        }

        if QUIET {
            for tgt in attacks & !self.occupancy() {
                if !self.square_attacked(tgt, blockers) {
                    move_list.push(Move::new(src, tgt, MoveType::Quiet));
                }
            }
        }
    }

    /// Generate castling moves for the given castling side.
    fn gen_castling_moves<const KS: usize>(&self, move_list: &mut MoveList) {
        const SRC: [Square; 2] = [Square::E1, Square::E8];
        const MID: [[Square; 2]; 2] = [[Square::F1, Square::F8], [Square::D1, Square::D8]];
        const TGT: [[Square; 2]; 2] = [[Square::G1, Square::G8], [Square::C1, Square::C8]];
        const OCCS: [[BitBoard; 2]; 2] = [
            [BitBoard(0x6000000000000000), BitBoard(0x0000000000000060)],
            [BitBoard(0x0E00000000000000), BitBoard(0x000000000000000E)],
        ];
        let side = self.side as usize;

        if self.occupancy() & OCCS[KS][side] == BitBoard::EMPTY
            && !self.square_attacked(MID[KS][side], self.occupancy())
            && !self.square_attacked(TGT[KS][side], self.occupancy())
        {
            move_list.push(Move::new(SRC[side], TGT[KS][side], MoveType::Castle));
        }
    }

    /// Generate all legal pawn advancing moves, including double pushes and promotions.
    fn gen_pawn_advances(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        move_list: &mut MoveList,
    ) {
        let pawns = self.own_pawns() & !diag_pins;
        let empty = !self.occupancy();
        let possible_targets = empty & check_mask;
        let single_pushes = pawns & possible_targets.forward(!self.side);

        // Handle promotions
        for src in single_pushes & BitBoard::PROMO_RANKS[self.side as usize] {
            let tgt = src.forward(self.side);
            if !hv_pins.get_bit(src) || hv_pins.get_bit(tgt) {
                move_list.push(Move::new(src, tgt, MoveType::QueenPromotion));
                move_list.push(Move::new(src, tgt, MoveType::KnightPromotion));
                move_list.push(Move::new(src, tgt, MoveType::RookPromotion));
                move_list.push(Move::new(src, tgt, MoveType::BishopPromotion));
            }
        }

        // Handle single pushes
        for src in single_pushes & !BitBoard::PROMO_RANKS[self.side as usize] {
            let tgt = src.forward(self.side);
            if !hv_pins.get_bit(src) || hv_pins.get_bit(tgt) {
                move_list.push(Move::new(src, tgt, MoveType::Quiet));
            }
        }

        // Handle double pushes
        let double_pushes = pawns
            & (empty & possible_targets.forward(!self.side)).forward(!self.side)
            & BitBoard::START_RANKS[self.side as usize];

        for src in double_pushes {
            let tgt = src.forward(self.side).forward(self.side);
            if !hv_pins.get_bit(src) || hv_pins.get_bit(tgt) {
                move_list.push(Move::new(src, tgt, MoveType::DoublePush));
            }
        }
    }

    /// Generate all pawn diagonal captures, including enpassant.
    fn gen_pawn_captures(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        move_list: &mut MoveList,
    ) {
        let pawns = self.own_pawns() & !hv_pins;
        let possible_targets = self.opp_occupancy() & check_mask;

        // Capture promotions
        for src in pawns & BitBoard::PROMO_RANKS[self.side as usize] {
            let mut attacks = pawn_attacks(src, self.side);

            if diag_pins.get_bit(src) {
                attacks &= diag_pins
            }

            for tgt in attacks & possible_targets {
                move_list.push(Move::new(src, tgt, MoveType::QueenCapPromo));
                move_list.push(Move::new(src, tgt, MoveType::KnightCapPromo));
                move_list.push(Move::new(src, tgt, MoveType::RookCapPromo));
                move_list.push(Move::new(src, tgt, MoveType::BishopCapPromo));
            }
        }

        // Enpassant
        if let Some(tgt) = self.en_passant {
            let ep_tgt = tgt.forward(!self.side);

            if check_mask & (tgt.to_board() | ep_tgt.to_board()) != BitBoard::EMPTY {
                for src in pawns & pawn_attacks(tgt, !self.side) {
                    // Restrict enpassant to the pin ray
                    if diag_pins.get_bit(src) && !diag_pins.get_bit(tgt) {
                        continue;
                    }

                    // En Passant discovered check!
                    let ep_rank = BitBoard::EP_RANKS[self.side as usize];
                    if self.own_king() & ep_rank != BitBoard::EMPTY
                        && self.opp_queen_rook() & ep_rank != BitBoard::EMPTY
                    {
                        // Remove the two pawns and see if the king sees an enemy orthogonal slider.
                        let blockers = self.occupancy() & !src.to_board() & !ep_tgt.to_board();
                        let king_square = self.own_king().lsb();
                        let king_ray = ep_rank & rook_attacks(king_square, blockers);

                        if king_ray & self.opp_queen_rook() != BitBoard::EMPTY {
                            continue;
                        }
                    }
                    move_list.push(Move::new(src, tgt, MoveType::EnPassant));
                }
            }
        }

        // Normal captures
        for src in pawns & !BitBoard::PROMO_RANKS[self.side as usize] {
            let mut attacks = pawn_attacks(src, self.side);

            if diag_pins.get_bit(src) {
                attacks &= diag_pins
            }

            for tgt in attacks & possible_targets {
                move_list.push(Move::new(src, tgt, MoveType::Capture));
            }
        }
    }

    /// Generate all legal moves for any standard piece.
    /// PIECE is either Piece::N, Piece::B, Piece::R. Rooks and Bishops also gen queen moves.
    fn gen_piece<const PIECE: usize, const QUIET: bool>(
        &self,
        check_mask: BitBoard,
        move_pin_mask: BitBoard,
        stuck_pin_mask: BitBoard,
        move_list: &mut MoveList,
    ) {
        let pieces = match PIECE {
            Piece::N => self.own_knights(),
            Piece::B => self.own_queens() | self.own_bishops(),
            Piece::R => self.own_queens() | self.own_rooks(),
            _ => unreachable!(),
        } & !stuck_pin_mask;
        let blockers = self.occupancy();

        for src in pieces {
            let mut attacks = match PIECE {
                Piece::N => knight_attacks(src),
                Piece::B => bishop_attacks(src, blockers),
                Piece::R => rook_attacks(src, blockers),
                _ => unreachable!(),
            } & check_mask;

            if move_pin_mask.get_bit(src) {
                attacks &= move_pin_mask;
            }

            for tgt in attacks & self.opp_occupancy() {
                move_list.push(Move::new(src, tgt, MoveType::Capture));
            }

            if QUIET {
                for tgt in attacks & !self.occupancy() {
                    move_list.push(Move::new(src, tgt, MoveType::Quiet));
                }
            }
        }
    }

    /// Generates pinned pieces and diagonal/orthogonal pin maps
    ///
    /// Pin masks are defined as the squares between a pinning enemy piece and one's own king.
    /// Any pinned piece can safely move along these squares (simply & moves with pinmask).
    /// For simplicity, pin masks also indirectly include the check mask (this has no actual
    /// effect on the pin use, as no piece can be sitting on the check mask anyways)
    fn map_pins(&self) -> (BitBoard, BitBoard) {
        let king_square = self.own_king().lsb();
        let occs = self.occupancy();

        // get all own pieces on diagonal/orthogonal rays from the king
        let possible_diag_pins = bishop_attacks(king_square, occs) & self.own_occupancy();
        let possible_hv_pins = rook_attacks(king_square, occs) & self.own_occupancy();

        // remove the possible pinned pieces
        let remove_diag_blockers = occs & !possible_diag_pins;
        let remove_hv_blockers = occs & !possible_hv_pins;

        // get all pinning pieces (pieces that see the king with pinned pieces removed)
        let diag_attackers =
            bishop_attacks(king_square, remove_diag_blockers) & self.opp_queen_bishop();
        let hv_attackers = rook_attacks(king_square, remove_hv_blockers) & self.opp_queen_rook();

        // pin masks are between the attacker and the king square (attacker included)
        let diag_pins = diag_attackers
            .into_iter()
            .map(|sq| BETWEEN[king_square as usize][sq as usize])
            .fold(BitBoard::EMPTY, |acc, x| acc | x);

        let hv_pins = hv_attackers
            .into_iter()
            .map(|sq| BETWEEN[king_square as usize][sq as usize])
            .fold(BitBoard::EMPTY, |acc, x| acc | x);

        (diag_pins, hv_pins)
    }

    /// Inner move generation, generic over the side to move.
    pub fn gen_moves<const QUIET: bool>(&self) -> MoveList {
        let mut ml = MoveList::default();
        let move_list = &mut ml;

        self.gen_king_moves::<QUIET>(move_list);

        // With double checks, only king moves are legal, so we stop here
        let attacker_count = self.checkers.count_bits();
        if attacker_count > 1 {
            return ml;
        }

        // Generate all the legal piece moves using pin and blocker/capture masks
        let check_mask = if attacker_count == 1 {
            BETWEEN[self.own_king().lsb() as usize][self.checkers.lsb() as usize] | self.checkers
        } else {
            BitBoard::FULL
        };
        let (diag_pins, hv_pins) = self.map_pins();
        let all_pins = diag_pins | hv_pins;

        if QUIET && attacker_count == 0 {
            if self.castling_rights.has_kingside(self.side) {
                self.gen_castling_moves::<KS>(move_list);
            }
            if self.castling_rights.has_queenside(self.side) {
                self.gen_castling_moves::<QS>(move_list);
            }
        }

        self.gen_pawn_captures(check_mask, diag_pins, hv_pins, move_list);
        if QUIET {
            self.gen_pawn_advances(check_mask, diag_pins, hv_pins, move_list);
        }

        self.gen_piece::<{ Piece::N }, QUIET>(check_mask, BitBoard::EMPTY, all_pins, move_list);
        self.gen_piece::<{ Piece::B }, QUIET>(check_mask, diag_pins, hv_pins, move_list);
        self.gen_piece::<{ Piece::R }, QUIET>(check_mask, hv_pins, diag_pins, move_list);

        ml
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
        self.pawns() & self.white() & pawn_attacks(square, Color::Black)
            | self.pawns() & self.black() & pawn_attacks(square, Color::White)
            | self.knights() & knight_attacks(square)
            | (self.bishops() | self.queens()) & bishop_attacks(square, blockers)
            | (self.rooks() | self.queens()) & rook_attacks(square, blockers)
            | self.kings() & king_attacks(square)
    }

    /// Returns the least valuable of the attackers within the attacker map
    fn get_lva(&self, attackers: BitBoard, side: Color) -> Option<(Square, Piece)> {
        let side_bb = self.side_bb[side as usize];

        for piece in Piece::SPLIT_COLOR[side as usize] {
            let squares = attackers & self.piece_bb[piece.index()] & side_bb;

            if squares != BitBoard::EMPTY {
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
        let mt = m.get_type();

        // Castling cannot have bad SEE, since all squares the king passes through are not attacked
        if mt == MoveType::Castle {
            return true;
        }

        // Piece being swapped off is the promoted piece
        let victim = if mt.is_promotion() {
            mt.get_promotion(self.side)
        } else {
            self.piece_at(src)
        };

        // Get the static move value (also works for quiets)
        let mut move_value = if mt.is_capture() {
            if mt == MoveType::EnPassant {
                PIECE_VALUES[Piece::WP as usize]
            } else {
                PIECE_VALUES[self.piece_at(tgt) as usize]
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

        let diagonal_sliders = self.bishops() | self.queens();
        let orthogonal_sliders = self.rooks() | self.queens();

        // Updated occupancy map after capture
        let mut occs = self.occupancy().pop_bit(src).set_bit(tgt);
        if mt == MoveType::EnPassant {
            let ep_tgt = self.en_passant.unwrap().forward(!self.side); // guaranteed to be Some
            occs = occs.pop_bit(ep_tgt);
        }

        // Get all pieces covering the exchange square and start exchanging
        let mut attackers = self.map_all_attackers(tgt, occs) & occs;
        let mut side_to_move = !self.side;

        loop {
            // SEE terminates when no recapture is possible.
            let own_attackers = attackers & self.side_bb[side_to_move as usize];
            if own_attackers == BitBoard::EMPTY {
                break;
            }

            // Get the least valuable attacker and simulate the recapture
            let (attacker_square, attacker) = self.get_lva(own_attackers, side_to_move).unwrap(); // attackers are at least one
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
                    && attackers & self.side_bb[side_to_move as usize] != BitBoard::EMPTY
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
        let board: Board = "R2bk3/5p2/4r1B1/1Q6/8/4Q3/4R3/2K5 b - - 0 1"
            .parse()
            .unwrap();
        println!("{board}");

        let (diag_pins, hv_pins) = board.map_pins();
        let pinned = (diag_pins | hv_pins) & board.own_occupancy();
        println!("{}\n{}\n{}", pinned, diag_pins, hv_pins);

        assert!(pinned.get_bit(Square::F7));
        assert!(pinned.get_bit(Square::E6));
        assert!(diag_pins.get_bit(Square::G6));
        assert!(hv_pins.get_bit(Square::C8));
        assert!(hv_pins.get_bit(Square::E3));
        assert!(!hv_pins.get_bit(Square::E2));
    }

    #[test]
    fn test_legal_pawn() {
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
        let b1: Board = "1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1"
            .parse()
            .unwrap();
        let b2: Board = "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1"
            .parse()
            .unwrap();

        let att1 = b1.map_all_attackers(Square::E5, b1.occupancy());
        let att2 = b2.map_all_attackers(Square::E5, b2.occupancy());

        println!("{b1}\n{att1}\n{b2}\n{att2}");

        assert!(att1.get_bit(Square::E1));
        assert!(!att1.get_bit(Square::D8));

        assert!(att2.get_bit(Square::E2));
        assert!(!att2.get_bit(Square::E1));
    }

    #[test]
    fn test_see() {
        #[rustfmt::skip]
        const SEE_SUITE: [(&str, &str, Eval, bool); 13] = [
            ("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1", "e1e5", 0, true),
            ("1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1", "d3e5", 0, false),
            ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", "g2h3", 0, true),
            ("k3r3/8/8/4p3/8/2B5/1B6/K7 w - - 0 1", "c3e5", 0, true),
            ("4kbnr/p1P4p/b1q5/5pP1/4n3/5Q2/PP1PPP1P/RNB1KBNR w KQk f6 0 1", "g5f6", 0, true),
            ("6k1/1pp4p/p1pb4/6q1/3P1pRr/2P4P/PP1Br1P1/5RKN w - - 0 1", "f1f4", 0, false),
            ("6RR/4bP2/8/8/5r2/3K4/5p2/4k3 w - - 0 1", "f7f8q", 0, true),
            ("r1bqk1nr/pppp1ppp/2n5/1B2p3/1b2P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1", "e1g1", 0, true),
            ("4kbnr/p1P1pppp/b7/4q3/7n/8/PPQPPPPP/RNB1KBNR w KQk - 0 1", "c7c8q", 0, true),
            ("4kbnr/p1P1pppp/b7/4q3/7n/8/PP1PPPPP/RNBQKBNR w KQk - 0 1", "c7c8q", 0, false),
            ("3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - - 0 1", "d3d4", 0, false),
            ("5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - - 0 1", "d6f4", 0, false),
            ("5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - - 0 1", "d6f4", -108, true),
        ];

        for (b, m, t, r) in SEE_SUITE {
            let board: Board = b.parse().unwrap();
            let m = board.find_move(m).unwrap();

            println!("Move: {m}{board}");
            assert_eq!(board.see(m, t), r);
        }
    }

    #[test]
    fn test_perft() {
        #[rustfmt::skip]
        const PERFT_SUITE: [(&str, &str, u64, usize); 16] = [
            ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "Startpos", 119060324, 6),
            ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", "Kiwipete", 193690690, 5),
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
            ("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", "Stalemate & checkmate #2", 23527, 4),
        ];

        for (fen, description, correct_count, depth) in PERFT_SUITE {
            let board: Board = fen.parse().unwrap();
            println!("{fen}\n{description}\n{board}");

            let nodes = board.perft(depth);
            assert_eq!(nodes, correct_count);
        }
    }
}
