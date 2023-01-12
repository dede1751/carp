/// Implements board representation and move generation
/// Any board without a king for each player (and with more than one for either) is UB!
use std::fmt;

use crate::{
    bitboard::*, castle::*, move_order::MoveList, moves::*, piece::*, square::*, tables::*,
    zobrist::*,
};

/// Starting position
pub const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
const FEN_LENGTH: usize = 6; // number of fen tokens

/// Piece-centric board representation
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
}

/// Pretty print board state
impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board_str: String =
            String::from("\n Board:\n\n\t┏━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┓");

        for rank in ALL_RANKS {
            board_str.push_str(format!("\n      {} ┃ ", 8 - rank as usize).as_str());

            for file in ALL_FILES {
                let square: Square = Square::from_coords(file, rank);
                let mut empty: bool = true;

                for piece in ALL_PIECES {
                    let board: BitBoard = self.pieces[piece as usize];
                    if board.get_bit(square) {
                        board_str.push_str(&piece.to_string());
                        board_str.push_str(" ┃");
                        empty = false;
                        break;
                    }
                }
                if empty {
                    board_str.push_str("  ┃");
                }
                board_str.push(' ');
            }
            if rank != Rank::First {
                board_str.push_str("\n\t┣━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━┫");
            }
        }
        board_str
            .push_str("\n\t┗━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┛\n\t  A   B   C   D   E   F   G   H\n");

        let en_passant_str = match self.en_passant {
            Some(square) => format!("{}", square),
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
impl TryFrom<&str> for Board {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let fen: Vec<&str> = value.split_whitespace().take(FEN_LENGTH).collect();
        if fen.len() != FEN_LENGTH {
            return Err("Invalid fen!");
        }

        let mut board: Board = Board::new();
        let board_str: &str = fen[0];
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
                    let piece = Piece::try_from(token)?;
                    board.set_piece(piece, Square::from_coords(file, rank));

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

        let rights = CastlingRights::try_from(fen[2])?;
        board.castling_rights = rights;
        board.hash.toggle_castle(rights);

        match fen[3] {
            "-" => board.en_passant = None,
            _ => {
                let ep_square = Square::try_from(fen[3])?;

                board.en_passant = Some(ep_square);
                board.hash.toggle_ep(ep_square);
            }
        }

        match fen[4].parse::<usize>() {
            Ok(hm) => board.halfmoves = hm,
            Err(_) => return Err("Invalid halfmove count!"),
        }

        Ok(board)
    }
}

/// Default to starting position
impl Default for Board {
    fn default() -> Self {
        Board::try_from(START_FEN).unwrap()
    }
}

/// Implement all piece bitboard lookups
macro_rules! impl_piece_lookups {
    ($($piece:ident, $own:ident, $opp:ident),*) => {
        $(impl Board {

            const fn $own(&self) -> BitBoard {
                self.pieces[self.side.$piece() as usize]
            }

            const fn $opp(&self) -> BitBoard {
                self.pieces[self.side.$piece().opposite_color() as usize]
            }
        })*
    };
}
impl_piece_lookups! {
    pawn, own_pawns, opp_pawns,
    knight, own_knights, opp_knights,
    bishop, own_bishops, opp_bishops,
    rook, own_rooks, opp_rooks,
    queen, own_queens, opp_queens,
    king, own_king, opp_king
}

/// Implement side occupancy and diagonal/hv slider lookups
impl Board {
    fn own_occupancy(&self) -> BitBoard {
        self.side_occupancy[self.side as usize]
    }
    fn opp_occupancy(&self) -> BitBoard {
        self.side_occupancy[!(self.side) as usize]
    }
    fn opp_queen_bishop(&self) -> BitBoard {
        self.opp_queens() | self.opp_bishops()
    }
    fn opp_queen_rook(&self) -> BitBoard {
        self.opp_queens() | self.opp_rooks()
    }
}

/// Implement board modification
impl Board {
    pub fn new() -> Board {
        Board {
            pieces: [EMPTY_BB; PIECE_COUNT],
            side_occupancy: [EMPTY_BB; 2],
            occupancy: EMPTY_BB,
            side: Color::White,
            castling_rights: NO_RIGHTS,
            en_passant: None,
            halfmoves: 0,
            hash: NULL_HASH,
        }
    }

    /// Set/remove piece while managing occupancy boards.
    /// Always remove first and set later!
    fn remove_piece(&mut self, piece: Piece, square: Square) {
        self.pieces[piece as usize].pop_bit(square);
        self.occupancy.pop_bit(square);
        self.side_occupancy[piece.color() as usize].pop_bit(square);
        self.hash.toggle_piece(piece, square);
    }
    fn set_piece(&mut self, piece: Piece, square: Square) {
        self.pieces[piece as usize].set_bit(square);
        self.occupancy.set_bit(square);
        self.side_occupancy[piece.color() as usize].set_bit(square);
        self.hash.toggle_piece(piece, square);
    }

    /// Makes (legal) move on the board
    /// Supplying illegal moves will lead to illegal board states.
    pub fn make_move(&self, m: Move) -> Board {
        let mut new: Board = self.clone();
        let (src, tgt): (Square, Square) = (m.get_src(), m.get_tgt());
        let piece: Piece = m.get_piece();
        let promotion: Piece = m.get_promotion();

        new.halfmoves += 1; // increment halfmove clock

        // always remove piece from source square
        new.remove_piece(piece, src);
        if piece == Piece::WP || piece == Piece::BP {
            new.halfmoves = 0
        } // halfmove clock reset

        // handle captures, enpassant or castling moves
        if m.is_enpassant() {
            let ep_target = PUSH[!self.side as usize][tgt as usize];

            new.remove_piece(m.get_capture(), ep_target);
        } else if m.is_capture() {
            new.remove_piece(m.get_capture(), tgt);
            new.halfmoves = 0; // halfmove clock reset
        } else if m.is_castle() {
            let rook = self.side.rook();
            let (rook_src, rook_tgt) = ROOK_CASTLING_MOVE[tgt as usize];

            new.remove_piece(rook, rook_src);
            new.set_piece(rook, rook_tgt);
        }

        // if promoting, set promotion piece, else set same piece (also change occupancies)
        if m.is_promotion() {
            new.set_piece(promotion, tgt);
        } else {
            new.set_piece(piece, tgt);
        }

        // remove old en passant square
        if let Some(square) = new.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        // if it's a double push, set enpassant square
        if m.is_double_push() {
            let ep_tgt = PUSH[self.side as usize][src as usize];

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        // handle changing castling rights
        let new_rights = self.castling_rights.update(src, tgt);

        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        // handle swapping side
        new.side = !self.side;
        new.hash.toggle_side();

        new
    }

    /// Pass turn to opponent, used for Null Move Pruning in the search
    pub fn make_null_move(&self) -> Board {
        let mut b = self.clone();

        b.side = !self.side;
        b.hash.toggle_side();

        if let Some(square) = self.en_passant {
            b.en_passant = None;
            b.hash.toggle_ep(square);
        }

        b
    }
}

/// Implement board move generation
impl Board {
    /// Checks whether the current side's king is in check
    ///
    /// Works on the basic idea that, if a certain square is attacked, if we put the attacking piece
    /// on the attacked square it will attack its old square. Hence we generate attacks on the
    /// target square for each piece, and check whether they land on any of the pieces stored
    /// in the board representation.
    pub fn king_in_check(&self, tables: &Tables) -> bool {
        let square = self.own_king().ls1b();

        self.opp_pawns()   & tables.get_pawn_attack(square, self.side) != EMPTY_BB || // pawns
        self.opp_knights() & tables.get_knight_attack(square)          != EMPTY_BB || // knights
        self.opp_queen_bishop() & tables.get_bishop_attack(square, self.occupancy) != EMPTY_BB || // bishops + queens
        self.opp_queen_rook()   & tables.get_rook_attack(square, self.occupancy)   != EMPTY_BB || // rooks + queens
        self.opp_king() & tables.get_king_attack(square)               != EMPTY_BB
        // kings
    }

    /// Gets bitboard with all enemy pieces directly attacking the king (same logic as king_in_check)
    fn map_king_attackers(&self, tables: &Tables) -> BitBoard {
        let square = self.own_king().ls1b();

        self.opp_pawns()   & tables.get_pawn_attack(square, self.side)             | // pawns
        self.opp_knights() & tables.get_knight_attack(square)                      | // knights
        self.opp_queen_bishop() & tables.get_bishop_attack(square, self.occupancy) | // bishops + queens
        self.opp_queen_rook()   & tables.get_rook_attack(square, self.occupancy)   | // rooks + queens
        self.opp_king() & tables.get_king_attack(square) // kings
    }

    /// Gets bitboard with all attacked squares by the opponent to see where the king can move
    ///
    /// We pretend the king is not on the board so that sliders also attack behind the king, since
    /// otherwise that square would be considered not attacked
    fn map_king_threats(&self, tables: &Tables) -> BitBoard {
        let king_square = self.own_king().ls1b();
        let mut occupancies = self.occupancy;
        occupancies.pop_bit(king_square);

        self.opp_pawns()
            .into_iter()
            .map(|sq| tables.get_pawn_attack(sq, !self.side))
            .fold(EMPTY_BB, |acc, x| acc | x)
            | self
                .opp_knights()
                .into_iter()
                .map(|sq| tables.get_knight_attack(sq))
                .fold(EMPTY_BB, |acc, x| acc | x)
            | self
                .opp_queen_bishop()
                .into_iter()
                .map(|sq| tables.get_bishop_attack(sq, occupancies))
                .fold(EMPTY_BB, |acc, x| acc | x)
            | self
                .opp_queen_rook()
                .into_iter()
                .map(|sq| tables.get_rook_attack(sq, occupancies))
                .fold(EMPTY_BB, |acc, x| acc | x)
            | self
                .opp_king()
                .into_iter()
                .map(|sq| tables.get_king_attack(sq))
                .fold(EMPTY_BB, |acc, x| acc | x)
    }

    /// Generates the diagonal and vertical pin masks
    ///
    /// Returns: (pinned pieces bb, diagonal pin bb, vertical pin bb)
    ///
    /// Pin masks are defined as the squares between a pinning enemy piece and one's own king.
    /// Any pinned piece can safely move along these squares (simply & moves with pinmask).
    /// For simplicity, pin masks also indirectly include the check mask (this has no actual
    /// effect on the pin use, as no piece can be sitting on the check mask anyways)
    fn map_pins(&self, tables: &Tables) -> (BitBoard, BitBoard, BitBoard) {
        let king_square = self.own_king().ls1b();

        // get all own pieces on diagonal/hv rays from the king
        let possible_diag_pins =
            tables.get_bishop_attack(king_square, self.occupancy) & self.own_occupancy();
        let possible_hv_pins =
            tables.get_rook_attack(king_square, self.occupancy) & self.own_occupancy();

        // remove the possible pinned pieces
        let remove_diag_blockers = self.occupancy & !possible_diag_pins;
        let remove_hv_blockers = self.occupancy & !possible_hv_pins;

        // get all pinning pieces (pieces that see the king with pinned pieces removed)
        let diag_attackers =
            tables.get_bishop_attack(king_square, remove_diag_blockers) & self.opp_queen_bishop();
        let hv_attackers =
            tables.get_rook_attack(king_square, remove_hv_blockers) & self.opp_queen_rook();

        // pin masks are between the attacker and the king square (attacker included)
        let diag_pins = diag_attackers
            .into_iter()
            .map(|sq| (BETWEEN[sq as usize][king_square as usize] | sq.to_board()))
            .fold(EMPTY_BB, |acc, x| acc | x);

        let hv_pins = hv_attackers
            .into_iter()
            .map(|sq| (BETWEEN[sq as usize][king_square as usize] | sq.to_board()))
            .fold(EMPTY_BB, |acc, x| acc | x);

        // pinned pieces are own pieces along any pin mask
        let pinned = (diag_pins | hv_pins) & self.own_occupancy();

        (pinned, diag_pins, hv_pins)
    }

    /// Looks for which piece was captured on tgt square
    /// Panics if no piece is set on the tgt square. Only call if it's sure to be a capture.
    fn get_captured_piece(&self, tgt: Square) -> Piece {
        (PIECES[!self.side as usize])
            .into_iter()
            .find(|&p| self.pieces[p as usize].get_bit(tgt))
            .unwrap() // possible panic
    }

    /// Converts attack bitboard to target squares and adds all the moves to the movelist
    fn add_moves(&self, piece: Piece, source: Square, attacks: BitBoard, move_list: &mut MoveList) {
        for target in attacks {
            if self.opp_occupancy().get_bit(target) {
                let captured_piece = self.get_captured_piece(target);

                move_list.add_capture(source, target, piece, captured_piece);
            } else {
                move_list.add_quiet(source, target, piece, 0);
            }
        }
    }

    /// Converts attack bitboard to target squares and adds all of them as captures to the movelist
    fn add_captures(
        &self,
        piece: Piece,
        source: Square,
        attacks: BitBoard,
        move_list: &mut MoveList,
    ) {
        for target in attacks {
            let captured_piece = self.get_captured_piece(target);

            move_list.add_capture(source, target, piece, captured_piece);
        }
    }

    /// Generate all legal king moves
    fn generate_king_moves(&self, threats: BitBoard, tables: &Tables, move_list: &mut MoveList) {
        let king_square = self.own_king().ls1b();
        let attacks = tables
            .get_king_attack(king_square) & // king moves
            !self.own_occupancy()         & // don't capture own pieces
            !threats; // avoid threats

        self.add_moves(self.side.king(), king_square, attacks, move_list);
    }

    /// Generate only legal king captures
    fn generate_king_captures(&self, threats: BitBoard, tables: &Tables, move_list: &mut MoveList) {
        let king_square = self.own_king().ls1b();
        let attacks = tables
            .get_king_attack(king_square) & // king moves
            self.opp_occupancy()          & // only consider captures
            !threats; // avoid threats

        self.add_captures(self.side.king(), king_square, attacks, move_list);
    }

    /// Generate all legal castling moves
    fn generate_castling_moves(&self, threats: BitBoard, move_list: &mut MoveList) {
        let side: usize = self.side as usize;
        let source = CASTLE_SQUARES[side];

        if self.castling_rights.has_kingside(self.side)
            && (threats | self.occupancy) & KINGSIDE_OCCUPANCIES[side] == EMPTY_BB
        {
            move_list.add_quiet(source, KINGSIDE_TARGETS[side], self.side.king(), 1);
        }

        if self.castling_rights.has_queenside(self.side)
            && self.occupancy & QUEENSIDE_OCCUPANCIES[side] == EMPTY_BB
            && threats & QUEENSIDE_THREATS[side] == EMPTY_BB
        {
            move_list.add_quiet(source, QUEENSIDE_TARGETS[side], self.side.king(), 1);
        }
    }

    /// Generate all legal pawn pushes
    fn generate_pawn_quiets(
        &self,
        blocker_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        move_list: &mut MoveList,
    ) {
        let side = self.side as usize;
        let pawn_bb = self.own_pawns() & !diag_pins; // diag pinned pawns cannot move

        for source in pawn_bb {
            let target: Square = PUSH[side][source as usize];

            // horizontally pinned pawns cannot move
            if hv_pins.get_bit(source) && !hv_pins.get_bit(target) {
                continue;
            }

            if !(self.occupancy.get_bit(target)) {
                // normal pawn push
                if blocker_mask.get_bit(target) {
                    move_list.add_pawn_quiet(source, target, self.side, 0);
                }

                // double pawn push
                if source.rank() == START_RANKS[side] {
                    let target = DOUBLE_PUSH[side][source.file() as usize];

                    if !(self.occupancy.get_bit(target)) && blocker_mask.get_bit(target) {
                        move_list.add_pawn_quiet(source, target, self.side, 1);
                    }
                }
            }
        }
    }

    /// Generate all legal pawn captures (including enpassant)
    fn generate_pawn_captures(
        &self,
        blocker_mask: BitBoard,
        capture_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let pawn_bb = self.own_pawns() & !hv_pins; // hv pinned pawns cannot capture
        let check_mask = capture_mask | blocker_mask;

        for source in pawn_bb {
            let mut attacks: BitBoard = tables.get_pawn_attack(source, self.side);

            // if pinned, only capture along diag pin ray (also goes for enpassant)
            if diag_pins.get_bit(source) {
                attacks &= diag_pins
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
                if attacks & ep_square.to_board() != EMPTY_BB
                    && (capture_mask.get_bit(ep_target) || blocker_mask.get_bit(ep_square))
                {
                    // En Passant discovered check!
                    let ep_rank = RANK_MASKS[ep_target as usize];

                    if ep_rank & self.own_king() != EMPTY_BB
                        && ep_rank & self.opp_queen_rook() != EMPTY_BB
                    {
                        // remove the two pawns
                        let occupancy = self.occupancy & !source.to_board() & !ep_target.to_board();

                        let king_square = self.own_king().ls1b();
                        let king_ray = tables.get_rook_attack(king_square, occupancy) & ep_rank;

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

    /// Generate all legal knight moves
    fn generate_knight_moves(
        &self,
        check_mask: BitBoard,
        pinned: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let knight_bb = self.own_knights() & !pinned; // pinned knights can never move

        for source in knight_bb {
            let attacks = tables.get_knight_attack(source) & // knight moves
                check_mask                                           & // cut moves that don't cover check
                !self.own_occupancy(); // cut moves capturing own pieces

            self.add_moves(self.side.knight(), source, attacks, move_list);
        }
    }

    /// Generate only legal knight captures
    fn generate_knight_captures(
        &self,
        check_mask: BitBoard,
        pinned: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let knight_bb = self.own_knights() & !pinned; // pinned knights can never move

        for source in knight_bb {
            let attacks = tables.get_knight_attack(source) & // knight moves
                check_mask                                           & // cut moves that don't cover check
                self.opp_occupancy(); // only consider captures

            self.add_captures(self.side.knight(), source, attacks, move_list);
        }
    }

    /// Generate all legal bishop moves
    fn generate_bishop_moves(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let bishop_bb = self.own_bishops() & !hv_pins; // hv pinned bishops can't move

        for source in bishop_bb {
            let mut attacks = tables
                .get_bishop_attack(source, self.occupancy)  & // bishop moves
                check_mask                                  & // cut moves that don't cover check
                !self.own_occupancy(); // cut moves capturing own pieces

            // if pinned, only move along the diagonal pin ray
            if diag_pins.get_bit(source) {
                attacks &= diag_pins
            }

            self.add_moves(self.side.bishop(), source, attacks, move_list);
        }
    }

    /// Generate only legal bishop captures
    fn generate_bishop_captures(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let bishop_bb = self.own_bishops() & !hv_pins; // hv pinned bishops can't move

        for source in bishop_bb {
            let mut attacks = tables
                .get_bishop_attack(source, self.occupancy)  & // bishop moves
                check_mask                                  & // cut moves that don't cover check
                self.opp_occupancy(); // only consider captures

            // if pinned, only move along the diagonal pin ray
            if diag_pins.get_bit(source) {
                attacks &= diag_pins
            }

            self.add_captures(self.side.bishop(), source, attacks, move_list);
        }
    }

    /// Generate all legal rook moves
    fn generate_rook_moves(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let rook_bb = self.own_rooks() & !diag_pins; // diag pinned rooks can't move

        for source in rook_bb {
            let mut attacks = tables
                .get_rook_attack(source, self.occupancy) & // rook moves
                check_mask                               & // cut moves that don't cover check
                !self.own_occupancy(); // cut moves capturing own pieces

            // if pinned, only move along hv pin ray
            if hv_pins.get_bit(source) {
                attacks &= hv_pins
            }

            self.add_moves(self.side.rook(), source, attacks, move_list);
        }
    }

    /// Generate only legal rook captures
    fn generate_rook_captures(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let rook_bb = self.own_rooks() & !diag_pins; // diag pinned rooks can't move

        for source in rook_bb {
            let mut attacks = tables
                .get_rook_attack(source, self.occupancy) & // rook moves
                check_mask                               & // cut moves that don't cover check
                self.opp_occupancy(); // only consider captures

            // if pinned, only move along hv pin ray
            if hv_pins.get_bit(source) {
                attacks &= hv_pins
            }

            self.add_captures(self.side.rook(), source, attacks, move_list);
        }
    }

    /// Generate all legal queen moves
    fn generate_queen_moves(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let queen_bb = self.own_queens();

        for source in queen_bb {
            let mut attacks = if diag_pins.get_bit(source) {
                // diagonal pin, only move like a bishop
                tables.get_bishop_attack(source, self.occupancy) & diag_pins
            } else if hv_pins.get_bit(source) {
                // hv pin, only move like a rook
                tables.get_rook_attack(source, self.occupancy) & hv_pins
            } else {
                // unpinned, move normally
                tables.get_queen_attack(source, self.occupancy)
            };
            attacks &= check_mask & !self.own_occupancy(); // handle check and avoid own pieces

            self.add_moves(self.side.queen(), source, attacks, move_list);
        }
    }

    /// Generate only legal queen captures
    fn generate_queen_captures(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList,
    ) {
        let queen_bb = self.own_queens();

        for source in queen_bb {
            let mut attacks = if diag_pins.get_bit(source) {
                // diagonal pin, only move like a bishop
                tables.get_bishop_attack(source, self.occupancy) & diag_pins
            } else if hv_pins.get_bit(source) {
                // hv pin, only move like a rook
                tables.get_rook_attack(source, self.occupancy) & hv_pins
            } else {
                // unpinned, move normally
                tables.get_queen_attack(source, self.occupancy)
            };
            attacks &= check_mask & self.opp_occupancy(); // handle check and only consider captures

            self.add_captures(self.side.queen(), source, attacks, move_list);
        }
    }

    /// Generate legal moves without make move.
    pub fn generate_moves(&self, tables: &Tables) -> MoveList {
        let mut move_list: MoveList = MoveList::new();
        let attackers = self.map_king_attackers(tables);
        let threats = self.map_king_threats(tables);
        let attacker_count = attackers.count_bits();

        let mut blocker_mask = !EMPTY_BB;
        let mut capture_mask = !EMPTY_BB;
        if attacker_count == 1 {
            let king_square = self.own_king().ls1b();

            blocker_mask = BETWEEN[king_square as usize][attackers.ls1b() as usize];
            capture_mask = attackers;
        }

        // generate all the legal king moves using king threats
        self.generate_king_moves(threats, tables, &mut move_list);

        // with double checks, only king moves are legal
        if attacker_count > 1 {
            return move_list;
        }

        // generate castling moves when not in check
        if self.castling_rights != NO_RIGHTS && attacker_count == 0 {
            self.generate_castling_moves(threats, &mut move_list);
        }

        // generate all the legal moves for pinned pieces
        let (pinned, diag, hv) = self.map_pins(tables);

        // generate all the legal piece moves using pin and blocker/capture masks
        let check_mask = blocker_mask | capture_mask;

        self.generate_pawn_captures(blocker_mask, capture_mask, diag, hv, tables, &mut move_list);
        self.generate_pawn_quiets(blocker_mask, diag, hv, &mut move_list);
        self.generate_knight_moves(check_mask, pinned, tables, &mut move_list);
        self.generate_bishop_moves(check_mask, diag, hv, tables, &mut move_list);
        self.generate_rook_moves(check_mask, diag, hv, tables, &mut move_list);
        self.generate_queen_moves(check_mask, diag, hv, tables, &mut move_list);

        move_list
    }

    /// Generate only legal captures without make move
    pub fn generate_captures(&self, tables: &Tables) -> MoveList {
        let mut move_list: MoveList = MoveList::new();
        let attackers = self.map_king_attackers(tables);
        let threats = self.map_king_threats(tables);
        let attacker_count = attackers.count_bits();

        let mut blocker_mask = !EMPTY_BB;
        let mut capture_mask = !EMPTY_BB;
        if attacker_count == 1 {
            let king_square = self.own_king().ls1b();

            blocker_mask = BETWEEN[king_square as usize][attackers.ls1b() as usize];
            capture_mask = attackers;
        }

        // generate all the legal king moves using king threats
        self.generate_king_captures(threats, tables, &mut move_list);

        // with double checks, only king moves are legal
        if attacker_count > 1 {
            return move_list;
        }

        // generate all the legal moves for pinned pieces
        let (pinned, diag, hv) = self.map_pins(tables);

        // generate all the legal piece moves using pin and blocker/capture masks
        let check_mask = blocker_mask | capture_mask;

        self.generate_pawn_captures(blocker_mask, capture_mask, diag, hv, tables, &mut move_list);
        self.generate_knight_captures(check_mask, pinned, tables, &mut move_list);
        self.generate_bishop_captures(check_mask, diag, hv, tables, &mut move_list);
        self.generate_rook_captures(check_mask, diag, hv, tables, &mut move_list);
        self.generate_queen_captures(check_mask, diag, hv, tables, &mut move_list);

        move_list
    }
}

/// Implement evaluation features
impl Board {
    /// Draw by insufficient material (strictly for when it is impossible to mate):
    ///     - King vs King
    ///     - King vs King + Bishop
    ///     - King vs King + Knight
    ///     - King + Bishop vs King + Bishop
    pub fn insufficient_material(&self) -> bool {
        match self.occupancy.count_bits() {
            2 => true,
            3 => {
                // bishop or knight left
                (self.pieces[Piece::WN as usize] | self.pieces[Piece::BN as usize]).count_bits()
                    == 1
                    || (self.pieces[Piece::WB as usize] | self.pieces[Piece::BB as usize])
                        .count_bits()
                        == 1
            }
            4 => {
                // opposite color bishops
                (self.pieces[Piece::WB as usize] | self.pieces[Piece::BB as usize]).count_bits()
                    == 2
                    && ((self.pieces[Piece::WB as usize] | self.pieces[Piece::BB as usize])
                        & WHITE_SQUARES)
                        .count_bits()
                        == 1
            }
            _ => false,
        }
    }

    /// Only king and pawns are on the board. Used to rule out null move pruning
    pub fn only_king_pawns_left(&self) -> bool {
        self.pieces[Piece::WN as usize] == EMPTY_BB
            && self.pieces[Piece::BN as usize] == EMPTY_BB
            && self.pieces[Piece::WB as usize] == EMPTY_BB
            && self.pieces[Piece::BB as usize] == EMPTY_BB
            && self.pieces[Piece::WR as usize] == EMPTY_BB
            && self.pieces[Piece::BR as usize] == EMPTY_BB
            && self.pieces[Piece::WQ as usize] == EMPTY_BB
            && self.pieces[Piece::BQ as usize] == EMPTY_BB
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invalid_fen() {
        let invalid_pieces =
            Board::try_from("rnbqkbnr/pp2ppppp/7/2p5/4P3/5N2/PPPP1P/RNBQKB1R b - - 1 2");
        let invalid_side =
            Board::try_from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1");
        let invalid_castle =
            Board::try_from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w kqKQ - 0 1");
        let invalid_ep_square =
            Board::try_from("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ a9 0 1");

        assert!(invalid_pieces.is_err());
        assert!(invalid_side.is_err());
        assert!(invalid_castle.is_err());
        assert!(invalid_ep_square.is_err());
    }

    #[test]
    fn test_pin_mask() {
        let tables: Tables = Tables::default();
        let board: Board = Board::try_from("R2bk3/5p2/4r1B1/1Q6/8/4Q3/4R3/2K5 b - - 0 1").unwrap();
        println!("{}", board);

        let (pinned, diag, hv) = board.map_pins(&tables);
        println!("{}\n{}\n{}", pinned, diag, hv);

        assert!(pinned.get_bit(Square::F7));
        assert!(pinned.get_bit(Square::E6));
        assert!(diag.get_bit(Square::G6));
        assert!(hv.get_bit(Square::C8));
        assert!(hv.get_bit(Square::E3));
        assert!(!hv.get_bit(Square::E2));
    }

    #[test]
    fn test_legal_pawn() {
        let tables: Tables = Tables::default();

        let b1: Board = Board::try_from("8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3 0 1").unwrap();
        println!("{}", b1);
        let m1 = b1.generate_moves(&tables); // enpassant blocks check
        assert_eq!(m1.len(), 6);

        let b2: Board = Board::try_from("8/8/8/2k5/3Pp3/8/8/4K3 b - d3 0 1").unwrap();
        println!("{}", b2);
        let m2 = b2.generate_moves(&tables); // enpassant captures checker
        assert_eq!(m2.len(), 9);

        let b3: Board = Board::try_from("8/8/8/8/k2Pp2Q/8/8/3K4 b - d3 0 1").unwrap();
        println!("{}", b3);
        let m3 = b3.generate_moves(&tables); // enpassant would leave the king in check
        assert_eq!(m3.len(), 6);
    }
}
