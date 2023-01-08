/// Implements board representation and move generation
use std::fmt;

use crate::{
    bitboard::*,
    square::*,
    piece::*, 
    castle::*, 
    moves::Move,
    move_order::MoveList,
    tables::*,
    zobrist::{ZHash, NULL_HASH},
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
        let mut board_str: String = String::from("\n Board:\n\n\t┏━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┓");

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
                if empty { board_str.push_str("  ┃"); }
                board_str.push(' ');
            }
            if rank != Rank::First { board_str.push_str("\n\t┣━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━┫"); }
        }
        board_str.push_str("\n\t┗━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┛\n\t  A   B   C   D   E   F   G   H\n");
    
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
            self.side,
            self.castling_rights,
            self.halfmoves,
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
        let mut token_count = 0;    // used for checking that number of tokens is correct
        
        let (mut file, mut rank) = (File::A, Rank::Eight);
        for token in board_str.chars() {
            match token {
                '/' => {
                    if token_count != 8 {
                        return Err("Invalid fen!");
                    };

                    rank = rank.down();
                    token_count = 0;
                },
                '1'..='8' => {
                    for _ in '1'..=token { 
                        file = file.right();
                        token_count += 1;
                    }
                },
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
             _  => return Err("Invalid fen!"),
        }

        let rights = CastlingRights::try_from(fen[2])?;
        board.castling_rights = rights;
        board.hash.toggle_castle(rights);

        match fen[3] {
            "-" => board.en_passant = None,
             _  => {
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
        $(
            impl Board {
                #[inline]
                fn $own(&self) -> BitBoard {
                    self.pieces[self.side.$piece() as usize]
                }
                #[inline]
                fn $opp(&self) -> BitBoard {
                    self.pieces[(!self.side).$piece() as usize]
                }
            }
        )*
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

/// Implement HV/Diagonal moving slider bitboard lookups
macro_rules! impl_slider_lookups {
    ($($piece:ident, $own:ident, $opp:ident),*) => {
        $(
            impl Board {
                #[inline]
                fn $own(&self) -> BitBoard {
                    self.own_queens() | self.pieces[self.side.$piece() as usize]
                }
                #[inline]
                fn $opp(&self) -> BitBoard {
                    self.opp_queens() | self.pieces[(!self.side).$piece() as usize]
                }
            }
        )*
    };
}
impl_slider_lookups! {
    bishop, own_queen_bishop, opp_queen_bishop,
    rook, own_queen_rook, opp_queen_rook
}

/// Implement side occupancy lookup
impl Board {
    #[inline]
    fn own_occupancy(&self) -> BitBoard {
        self.side_occupancy[self.side as usize]
    }
    #[inline]
    fn opp_occupancy(&self) -> BitBoard {
        self.side_occupancy[!(self.side) as usize]
    }
}


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
    #[inline]
    fn remove_piece(&mut self, piece: Piece, square: Square) {
        self.pieces[piece as usize].pop_bit(square);
        self.occupancy.pop_bit(square);
        self.side_occupancy[piece.color() as usize].pop_bit(square);
        self.hash.toggle_piece(piece, square);
    }
    #[inline]
    fn set_piece(&mut self, piece: Piece, square: Square) {
        self.pieces[piece as usize].set_bit(square);
        self.occupancy.set_bit(square);
        self.side_occupancy[piece.color() as usize].set_bit(square);
        self.hash.toggle_piece(piece, square);
    }

    /// Checks whether a certain square is attacked by the opponent
    /// 
    /// Works on the basic idea that, if a certain square is attacked, if we put the attacking piece
    /// on the attacked square it will attack its old square. Hence we generate attacks on the
    /// target square for each piece, and check whether they land on any of the pieces stored
    /// in the board representation.
    /// 
    /// Does not handle pawn promotions/capture promotions!

    #[inline]
    fn is_square_attacked(&self, square: Square, tables: &Tables) -> bool {
        self.opp_pawns()   & tables.get_pawn_attack(square, self.side) != EMPTY_BB || // pawns
        self.opp_knights() & tables.get_knight_attack(square)          != EMPTY_BB || // knights
        self.opp_queen_bishop() & tables.get_bishop_attack(square, self.occupancy) != EMPTY_BB || // bishops + queens
        self.opp_queen_rook()   & tables.get_rook_attack(square, self.occupancy)   != EMPTY_BB || // rooks + queens
        self.opp_king() & tables.get_king_attack(square)               != EMPTY_BB    // kings
    }

    /// Checks whether the current side's king is in check
    #[inline]
    pub fn king_in_check(&self, tables: &Tables) -> bool {
        let king_square = self.own_king().ls1b();

        self.is_square_attacked(king_square, tables)
    }

    /// Gets bitboard with all king attackers
    #[inline]
    fn map_king_attackers(&self, tables: &Tables) -> BitBoard {
        let square = self.own_king().ls1b();

        self.opp_pawns()   & tables.get_pawn_attack(square, self.side)             | // pawns
        self.opp_knights() & tables.get_knight_attack(square)                      | // knights
        self.opp_queen_bishop() & tables.get_bishop_attack(square, self.occupancy) | // bishops + queens
        self.opp_queen_rook()   & tables.get_rook_attack(square, self.occupancy)   | // rooks + queens
        self.opp_king() & tables.get_king_attack(square)                             // kings
    }

    /// Gets bitboard with all attacked squares by the opponent to see where the king can move
    /// 
    /// We pretend the king is not on the board so that sliders also attack behind the king, since
    /// otherwise that square would be considered not attacked
    #[inline]
    fn map_king_threats(&self, tables: &Tables) -> BitBoard {
        let king_square = self.own_king().ls1b();
        let mut occupancies = self.occupancy;
        occupancies.pop_bit(king_square);
        
        self.opp_pawns().into_iter()
            .map(|sq| tables.get_pawn_attack(sq, !self.side))
            .fold(EMPTY_BB, |acc, x| acc | x)           |

        self.opp_knights().into_iter()
            .map(|sq| tables.get_knight_attack(sq))
            .fold(EMPTY_BB, |acc, x| acc | x)           |

        self.opp_queen_bishop().into_iter()
            .map(|sq| tables.get_bishop_attack(sq, occupancies))
            .fold(EMPTY_BB, |acc, x| acc | x)           |

        self.opp_queen_rook().into_iter()
            .map(|sq| tables.get_rook_attack(sq, occupancies))
            .fold(EMPTY_BB, |acc, x| acc | x)           |

        self.opp_king().into_iter()
            .map(|sq| tables.get_king_attack(sq))
            .fold(EMPTY_BB, |acc, x| acc | x) 
    }

    /// Generates the diagonal and vertical pin masks
    /// 
    /// Returns: (pinned pieces bb, diagonal pin bb, vertical pin bb)
    /// 
    /// Pin masks are defined as the squares between a pinning enemy piece and one's own king.
    /// Any pinned piece can safely move along these squares (simply & moves with pinmask)
    /// 
    /// Pinned rooks can only move on hv pins
    /// Pinned bishops can only move on diagonal pins
    /// Pinned knights cannot move
    /// Pinned pawns can only advance on hv pins
    /// Pinned pawns can only capture on diagonal pins
    /// Pinned queens behave either like bishops or rooks, depending on pin
    /// Any piece can only be pinned along a single ray
    fn map_pins(&self, tables: &Tables) -> (BitBoard, BitBoard, BitBoard) {
        let king_square = self.own_king().ls1b();

        // get all own pieces on diagonal/hv rays from the king
        let possible_diag_pins = tables.get_bishop_attack(king_square, self.occupancy) & self.own_occupancy();
        let possible_hv_pins = tables.get_rook_attack(king_square, self.occupancy) & self.own_occupancy();

        // remove the possible pinned pieces
        let remove_diag_blockers = self.occupancy & !possible_diag_pins;
        let remove_hv_blockers = self.occupancy & !possible_hv_pins;

        // get rays from king without the possibly pinned pieces
        let king_bishop_rays = tables.get_bishop_attack(king_square, remove_diag_blockers);
        let king_rook_rays = tables.get_rook_attack(king_square, remove_hv_blockers);

        // squares with diag/hv pinning enemy pieces
        let diag_attackers = self.opp_queen_bishop() & king_bishop_rays;
        let hv_attackers = self.opp_queen_rook() & king_rook_rays;
        
        // pin masks are between the attacker and the king square (attacker included)
        let diag_pins = diag_attackers.into_iter()
                .map(|sq| BETWEEN[sq as usize][king_square as usize] | sq.to_board())
                //.filter(|bb| *bb & pinned != EMPTY_BB )
                .fold(EMPTY_BB, |acc, x| acc | x);
        
        let hv_pins = hv_attackers.into_iter()
                .map(|sq| BETWEEN[sq as usize][king_square as usize] | sq.to_board())
                //.filter(|bb| *bb & pinned != EMPTY_BB )
                .fold(EMPTY_BB, |acc, x| acc | x);
        // let enemy_bishop_rays = diag_attackers.into_iter()
        //     .map(|sq| tables.get_bishop_attack(sq, self.occupancy))
        //     .fold(EMPTY_BB, |acc, x| acc | x);

        // let enemy_rook_rays = hv_attackers.into_iter()
        //     .map(|sq| tables.get_rook_attack(sq, self.occupancy))
        //     .fold(EMPTY_BB, |acc, x| acc | x);

        // pinned pieces are the pieces along the pin masks
        let pinned = (diag_pins | hv_pins) & self.own_occupancy();

        (pinned, diag_pins, hv_pins)
    }

    /// Looks for which piece was captured on tgt square
    /// 
    /// Panics if no piece is set on the tgt square. Only call if it's sure to be a capture.
    #[inline]
    fn get_captured_piece(&self, tgt: Square) -> Piece {
        match self.side {
            Color::White => *BLACK_PIECES.iter()
                .find(|&p| { self.pieces[*p as usize].get_bit(tgt) })
                .unwrap(), // possible panic
            Color::Black => *WHITE_PIECES.iter()
                .find(|&p| { self.pieces[*p as usize].get_bit(tgt) })
                .unwrap(), // possible panic
        }
    }
    
    /// Generate all pawns moves
    fn generate_pawn_moves(&self, tables: &Tables, move_list: &mut MoveList) {
        let (piece,
            promotions,
            promote_rank,
            start_rank
        ) = match self.side {
            Color::White => (Piece::WP, WHITE_PROMOTIONS, Rank::Seventh, Rank::Second),
            Color::Black => (Piece::BP, BLACK_PROMOTIONS, Rank::Second, Rank::Seventh),
        };

        for source in self.pieces[piece as usize] {
            // quiets
            let target: Square = match self.side{
                Color::White => source.up(),
                Color::Black => source.down(),
            };

            if !(self.occupancy.get_bit(target)) {
                if source.rank() == promote_rank { // promotion
                    for promotion in promotions {
                        move_list.add_quiet(source, target, piece, promotion, 0, 0);
                    }
                } else {
                    // normal pawn push
                    move_list.add_quiet(source, target, piece, Piece::WP, 0, 0);

                    // double pawn push
                    if source.rank() == start_rank {
                        let double_push = match self.side {
                            Color::White => target.up(),
                            Color::Black => target.down(),
                        };
    
                        if !(self.occupancy.get_bit(double_push)){
                            move_list.add_quiet(source, double_push, piece, Piece::WP, 1, 0);
                        }
                    }
                }
            }
            // attacks
            let attacks: BitBoard = tables.get_pawn_attack(source, self.side);
            let captures: BitBoard = attacks & self.side_occupancy[(!self.side) as usize];

            if source.rank() == promote_rank { // capture promotion
                for target in captures {
                    for promotion in promotions {
                        let captured_piece = self.get_captured_piece(target);
    
                        move_list.add_capture(
                            source, target, piece, captured_piece, promotion, 0
                        );
                    }
                }
            } else {    // normal/enpassant capture
                for target in captures {
                    let captured_piece = self.get_captured_piece(target);
    
                    move_list.add_capture(
                        source, target, piece, captured_piece, Piece::WP, 0
                    );
                }

                if let Some(ep_square) = self.en_passant {
                    if (attacks & ep_square.to_board()) != EMPTY_BB {
                        move_list.add_capture(
                            source, ep_square, piece, piece.opposite_color(), Piece::WP, 1
                        );
                    }
                }
            }
        }        
    }

    /// Generate pseudolegal castling moves (king can be left in check)
    fn generate_castling_moves(&self, tables: &Tables, move_list: &mut MoveList) {
        let side: usize = self.side as usize;
        let source = CASTLE_SQUARES[side];

        if self.is_square_attacked(source, tables) { return }; // no castle in check

        if  self.castling_rights.has_kingside(self.side)                             &&
            self.occupancy & KINGSIDE_OCCUPANCIES[side] == EMPTY_BB                  &&
            !self.is_square_attacked(KINGSIDE_SQUARES[side], tables)
        {
            move_list.add_quiet(source, KINGSIDE_TARGETS[side], self.side.king(), Piece::WP, 0, 1);
        }

        if  self.castling_rights.has_queenside(self.side)                            &&
            self.occupancy & QUEENSIDE_OCCUPANCIES[side] == EMPTY_BB                 &&
            !self.is_square_attacked(QUEENSIDE_SQUARES[side], tables)
        {
            move_list.add_quiet(source, QUEENSIDE_TARGETS[side], self.side.king(), Piece::WP, 0, 1);
        }
    }

    /// Generate "conventional" moves for all pieces except pawns through attack tables
    fn generate_piece_moves(&self, piece: Piece, tables: &Tables, move_list: &mut MoveList) {
        let piece_bb: BitBoard = self.pieces[piece as usize];
        let blockers: BitBoard = self.occupancy;

        for source in piece_bb {
            let attacks = match (piece as usize) & PIECE_BITS {
                KNIGHT => tables.get_knight_attack(source),
                BISHOP => tables.get_bishop_attack(source, blockers),
                ROOK => tables.get_rook_attack(source, blockers),
                QUEEN => tables.get_queen_attack(source, blockers),
                KING => tables.get_king_attack(source),
                _ => EMPTY_BB,
            };

            for target in attacks {
                if self.side_occupancy[(!self.side) as usize].get_bit(target) {
                    let captured_piece = self.get_captured_piece(target);
    
                    move_list.add_capture(
                        source, target, piece, captured_piece, Piece::WP, 0
                    );
                } else if !self.side_occupancy[self.side as usize].get_bit(target) {
                    move_list.add_quiet(source, target, piece, Piece::WP, 0, 0);
                }
            }
        }
    }

    /// Generate all pseudo-legal moves for the current side.
    ///
    /// Assumes position legality according to the starting position, so for example pawns can only
    /// be between 2nd and 7th and when having castling rights the king is on the start square.
    /// This generates all moves, including those leaving the king in check.
    pub fn generate_moves(&self, tables: &Tables) -> MoveList {
        let mut move_list: MoveList = MoveList::new();

        self.generate_pawn_moves(tables, &mut move_list);
        self.generate_castling_moves(tables, &mut move_list);

        match self.side {
            Color::White => {
                for &piece in &WHITE_PIECES[1..] {
                    self.generate_piece_moves(piece, tables, &mut move_list);
                }
            }
            Color::Black => {
                for &piece in &BLACK_PIECES[1..] {
                    self.generate_piece_moves(piece, tables, &mut move_list);
                }
            }
        }

        move_list
    }

    #[inline]
    fn add_moves(&self, piece: Piece, source: Square, attacks: BitBoard, move_list: &mut MoveList) {
        for target in attacks {
            if self.opp_occupancy().get_bit(target) {
                let captured_piece = self.get_captured_piece(target);

                move_list.add_capture(source, target, piece, captured_piece, Piece::WP, 0);
            } else {
                move_list.add_quiet(source, target, piece, Piece::WP, 0, 0);
            }
        }
    }

    #[inline]
    fn generate_legal_king(&self, threats: BitBoard, tables: &Tables, move_list: &mut MoveList) {
        let king_square = self.own_king().ls1b();
        let attacks = tables
            .get_king_attack(king_square) & // king moves
            !self.own_occupancy()         & // don't capture own pieces
            !threats;                       // avoid threats

        self.add_moves(self.side.king(), king_square, attacks, move_list);
    }

    #[inline]
    fn generate_legal_castle(&self, threats: BitBoard, move_list: &mut MoveList) {
        let side: usize = self.side as usize;
        let source = CASTLE_SQUARES[side];

        if  self.castling_rights.has_kingside(self.side)                            &&
            (threats | self.occupancy) & KINGSIDE_OCCUPANCIES[side] == EMPTY_BB
        {
            move_list.add_quiet(source, KINGSIDE_TARGETS[side], self.side.king(), Piece::WP, 0, 1);
        }

        if  self.castling_rights.has_queenside(self.side)                           &&
            self.occupancy & QUEENSIDE_OCCUPANCIES[side] == EMPTY_BB                &&
            threats & QUEENSIDE_THREATS[side] == EMPTY_BB
        {
            move_list.add_quiet(source, QUEENSIDE_TARGETS[side], self.side.king(), Piece::WP, 0, 1);
        }
    }

    #[inline]
    fn generate_pawn_quiets(
        &self,
        blocker_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        move_list: &mut MoveList
    ) {
        let (piece,
            promotions,
            promote_rank,
            start_rank
        ) = match self.side {
            Color::White => (Piece::WP, WHITE_PROMOTIONS, Rank::Seventh, Rank::Second),
            Color::Black => (Piece::BP, BLACK_PROMOTIONS, Rank::Second, Rank::Seventh),
        };

        let pawn_bb = self.own_pawns() & !diag_pins; // diag pinned pawns cannot move
        
        for source in pawn_bb {
            let target: Square = match self.side{
                Color::White => source.up(),
                Color::Black => source.down(),
            };

            // horizontally pinned pawns cannot move
            if  hv_pins.get_bit(source) && !hv_pins.get_bit(target) { continue; }

            if !(self.occupancy.get_bit(target)) {
                if source.rank() == promote_rank { // promotion
                    if blocker_mask.get_bit(target) {
                        for promotion in promotions {
                            move_list.add_quiet(source, target, piece, promotion, 0, 0);
                        }
                    }
                } else {
                    // normal pawn push
                    if blocker_mask.get_bit(target) {
                        move_list.add_quiet(source, target, piece, Piece::WP, 0, 0);
                    }

                    // double pawn push
                    if source.rank() == start_rank {
                        let double_push = match self.side {
                            Color::White => target.up(),
                            Color::Black => target.down(),
                        };
    
                        if !(self.occupancy.get_bit(double_push)) &&
                            blocker_mask.get_bit(double_push)
                        {
                            move_list.add_quiet(source, double_push, piece, Piece::WP, 1, 0);
                        }
                    }
                }
            }
        }
    }

    #[inline]
    fn generate_pawn_captures(
        &self,
        blocker_mask: BitBoard,
        capture_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList
    ) {
        let (piece, promotions, promote_rank) = match self.side {
            Color::White => (Piece::WP, WHITE_PROMOTIONS, Rank::Seventh),
            Color::Black => (Piece::BP, BLACK_PROMOTIONS, Rank::Second),
        };

        let pawn_bb = self.own_pawns() & !hv_pins; // hv pinned pawns cannot capture
        let check_mask = capture_mask | blocker_mask;

        for source in pawn_bb {
            let mut attacks: BitBoard = tables.get_pawn_attack(source, self.side);
            
            // if pinned, only capture along diag pin ray (also goes for enpassant)
            if diag_pins.get_bit(source) { attacks &= diag_pins }
            
            let captures: BitBoard = attacks & check_mask & self.opp_occupancy();

            if source.rank() == promote_rank {
                // capture promotion
                for target in captures {
                    for promotion in promotions {
                        let captured_piece = self.get_captured_piece(target);
    
                        move_list.add_capture(source, target, piece, captured_piece, promotion, 0);
                    }
                }
            } else {
                // normal/enpassant capture
                for target in captures {
                    let captured_piece = self.get_captured_piece(target);
    
                    move_list.add_capture(source, target, piece, captured_piece, Piece::WP, 0);
                }

                if let Some(ep_square) = self.en_passant {
                    let ep_target = match self.side {
                        Color::White => ep_square.down(),
                        Color::Black => ep_square.up(),
                    };

                    // Attack must land on ep square, and either capture the checking piece or
                    // block the check
                    if attacks & ep_square.to_board() != EMPTY_BB &&
                        (capture_mask.get_bit(ep_target) || blocker_mask.get_bit(ep_square))                      
                    {

                        // En Passant discovered check!
                        let ep_rank = RANK_MASKS[ep_target as usize];

                        if  ep_rank & self.own_king()       != EMPTY_BB &&
                            ep_rank & self.opp_queen_rook() != EMPTY_BB
                        {
                            // remove the two pawns
                            let occupancy = self.occupancy &
                                !source.to_board()                   &
                                !ep_target.to_board();
                            
                            let king_square = self.own_king().ls1b();
                            let king_ray = tables.get_rook_attack(king_square, occupancy) & ep_rank;

                            // king sees enemy queen or rook directly
                            if king_ray & self.opp_queen_rook() != EMPTY_BB { continue; }
                        }

                        move_list.add_capture(
                            source, ep_square, piece, piece.opposite_color(), Piece::WP, 1
                        );
                    }
                }
            }
        }
    }

    #[inline]
    fn generate_legal_knight(
        &self,
        check_mask: BitBoard,
        pinned: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList
    ) {
        let knight_bb = self.own_knights() & !pinned; // pinned knights can never move

        for source in knight_bb {
            let attacks = tables.get_knight_attack(source) & // knight moves
                check_mask                                           & // cut moves that don't cover check
                !self.own_occupancy();                                 // cut moves capturing own pieces

            self.add_moves(self.side.knight(), source, attacks, move_list);
        }
    }

    #[inline]
    fn generate_legal_bishop(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList
    ) {
        let bishop_bb = self.own_bishops() & !hv_pins; // hv pinned bishops can't move

        for source in bishop_bb {
            let mut attacks = tables
                .get_bishop_attack(source, self.occupancy)  & // bishop moves
                check_mask                                  & // cut moves that don't cover check
                !self.own_occupancy();                        // cut moves capturing own pieces
            
            // if pinned, only move along the diagonal pin ray
            if diag_pins.get_bit(source) { attacks &= diag_pins }

            self.add_moves(self.side.bishop(), source, attacks, move_list);
        }
    }

    #[inline]
    fn generate_legal_rook(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList
    ) {
        let rook_bb = self.own_rooks() & !diag_pins; // diag pinned rooks can't move

        for source in rook_bb {
            let mut attacks = tables
                .get_rook_attack(source, self.occupancy) & // bishop moves
                check_mask                               & // cut moves that don't cover check
                !self.own_occupancy();                     // cut moves capturing own pieces
        
            // if pinned, only move along hv pin ray
            if hv_pins.get_bit(source) { attacks &= hv_pins }

            self.add_moves(self.side.rook(), source, attacks, move_list);
        }
    }

    #[inline]
    fn generate_legal_queen(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        tables: &Tables,
        move_list: &mut MoveList
    ) {
        let queen_bb = self.own_queens();

        for source in queen_bb {
            let mut attacks = if diag_pins.get_bit(source) {
                // diagonal pin, only move like a bishop
                tables.get_bishop_attack(source, self.occupancy) & diag_pins
            } else if hv_pins.get_bit(source) {
                // hv pin, only move like a rook
                tables.get_rook_attack(source, self.occupancy)   & hv_pins
            } else {
                // unpinned, move normally
                tables.get_queen_attack(source, self.occupancy)
            };
            attacks &= check_mask & !self.own_occupancy(); // handle check and avoid own pieces

            self.add_moves(self.side.queen(), source, attacks, move_list);
        }
    }

    /// Generate legal moves without make move.
    pub fn generate_legal(&self, tables: &Tables) -> MoveList {
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
        self.generate_legal_king(threats, tables, &mut move_list);

        // with double checks, only king moves are legal
        if attacker_count > 1 { return move_list; }

        // generate castling moves when not in check
        if self.castling_rights != NO_RIGHTS && attacker_count == 0 {
            self.generate_legal_castle(threats, &mut move_list);
        }
        
        // generate all the legal moves for pinned pieces
        let (pinned, diag, hv) = self.map_pins(tables);

        // generate all the legal piece moves using pin and blocker/capture masks
        let check_mask = blocker_mask | capture_mask;

        self.generate_pawn_captures(blocker_mask, capture_mask, diag, hv, tables, &mut move_list);
        self.generate_pawn_quiets(blocker_mask, diag, hv, &mut move_list);
        self.generate_legal_knight(check_mask, pinned, tables, &mut move_list);
        self.generate_legal_bishop(check_mask, diag, hv, tables, &mut move_list);
        self.generate_legal_rook(check_mask, diag, hv, tables, &mut move_list);
        self.generate_legal_queen(check_mask, diag, hv, tables, &mut move_list);

        move_list
    }

    /// Clone current board state and perform a pseudolegal move. None if move is illegal.
    /// Requires that supplied moves be pseudolegal moves.
    /// 
    /// Panics if:
    /// Move is flagged as capture but no piece to capture is found.
    /// Move is castle but target is neither G1, C1, G8 or C8
    /// 
    /// Copy/Make approach is achieved simply by cloning the board state
    pub fn make_move(&self, m: Move, tables: &Tables) -> Option<Board> {
        let mut new: Board = self.clone();
        let (src, tgt): (Square, Square) = (m.get_src(), m.get_tgt());
        let piece: Piece = m.get_piece();
        let promotion: Piece = m.get_promotion();
        
        new.halfmoves += 1; // increment halfmove clock

        // always remove piece from source square
        new.remove_piece(piece, src);
        if piece == Piece::WP || piece == Piece::BP { new.halfmoves = 0 } // halfmove clock reset

        // handle captures, enpassant or castling moves
        if m.is_enpassant() { // enpassant
            match self.side {
                Color::White => new.remove_piece(Piece::BP, tgt.down()), 
                Color::Black => new.remove_piece(Piece::WP, tgt.up()),
            }
            
        } else if m.is_capture() { // normal capture
            new.remove_piece(m.get_capture(), tgt);
            new.halfmoves = 0;     // halfmove clock reset

        } else if m.is_castle() {  // castling
            
            match tgt {
                Square::G1 => {
                    new.remove_piece(Piece::WR, Square::H1);
                    new.set_piece(Piece::WR, Square::F1);
                },
                Square::C1 => {
                    new.remove_piece(Piece::WR, Square::A1);
                    new.set_piece(Piece::WR, Square::D1);
                },
                Square::G8 => {
                    new.remove_piece(Piece::BR, Square::H8);
                    new.set_piece(Piece::BR, Square::F8);
                },
                Square::C8 => {
                    new.remove_piece(Piece::BR, Square::A8);
                    new.set_piece(Piece::BR, Square::D8);
                },
                _ => unreachable!(), // technically unreachable
            };
        } 

        // if promoting, set promotion piece, else set same piece (also change occupancies)
        if m.is_promotion() {
            new.set_piece(promotion, tgt);
        } else {
            new.set_piece(piece, tgt);
        }
        
        // legality check after changing occupancies
        if new.king_in_check(tables) { return None }
        
        // remove old en passant square
        if let Some(square) = new.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        // if it's a double push, set enpassant square
        if m.is_double_push() {
            let ep_tgt = match self.side {
                Color::White => src.up(),
                Color::Black => src.down(),
            };

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
    
        Some(new)
    }

    /// Pass turn to opponent, used for Null Move Pruning in the search
    #[inline]
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

    pub fn make_m(&self, m: Move) -> Board {
        let mut new: Board = self.clone();
        let (src, tgt): (Square, Square) = (m.get_src(), m.get_tgt());
        let piece: Piece = m.get_piece();
        let promotion: Piece = m.get_promotion();
        
        new.halfmoves += 1; // increment halfmove clock

        // always remove piece from source square
        new.remove_piece(piece, src);
        if piece == Piece::WP || piece == Piece::BP { new.halfmoves = 0 } // halfmove clock reset

        // handle captures, enpassant or castling moves
        if m.is_enpassant() { // enpassant
            match self.side {
                Color::White => new.remove_piece(Piece::BP, tgt.down()), 
                Color::Black => new.remove_piece(Piece::WP, tgt.up()),
            }
            
        } else if m.is_capture() { // normal capture
            new.remove_piece(m.get_capture(), tgt);
            new.halfmoves = 0;     // halfmove clock reset

        } else if m.is_castle() {  // castling
            
            match tgt {
                Square::G1 => {
                    new.remove_piece(Piece::WR, Square::H1);
                    new.set_piece(Piece::WR, Square::F1);
                },
                Square::C1 => {
                    new.remove_piece(Piece::WR, Square::A1);
                    new.set_piece(Piece::WR, Square::D1);
                },
                Square::G8 => {
                    new.remove_piece(Piece::BR, Square::H8);
                    new.set_piece(Piece::BR, Square::F8);
                },
                Square::C8 => {
                    new.remove_piece(Piece::BR, Square::A8);
                    new.set_piece(Piece::BR, Square::D8);
                },
                _ => unreachable!(), // technically unreachable
            };
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
            let ep_tgt = match self.side {
                Color::White => src.up(),
                Color::Black => src.down(),
            };

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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invalid_fen() {
        let invalid_pieces = Board::try_from(
            "rnbqkbnr/pp2ppppp/7/2p5/4P3/5N2/PPPP1P/RNBQKB1R b - - 1 2");
        let invalid_side = Board::try_from(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1");
        let invalid_castle = Board::try_from(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w kqKQ - 0 1");
        let invalid_ep_square = Board::try_from(
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ a9 0 1");

        assert!(invalid_pieces.is_err());
        assert!(invalid_side.is_err());
        assert!(invalid_castle.is_err());
        assert!(invalid_ep_square.is_err());
    }

    #[test]
    fn test_square_attacks() {
        let tables: Tables = Tables::default();
        let b1: Board = Board::try_from(
            "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"
        ).unwrap();

        // println!("{}", b1.is_square_attacked(&tables, Square::E5, Color::White));

        // assert!(b1.is_square_attacked(&tables, Square::E5, Color::White)); // attacked by white pawn
        // assert!(!(b1.is_square_attacked(&tables, Square::E4, Color::Black)));// only attacked by white
        // assert!(b1.is_square_attacked(&tables, Square::G7, Color::Black)); // black bishop attacking white pawn
        // assert!(b1.is_square_attacked(&tables, Square::A7, Color::Black)); // black rook attacking black pawn
        // assert!(b1.is_square_attacked(&tables, Square::E3, Color::White)); // white queen attacking white pawn
    }

    #[test]
    fn test_pseudolegal_generation() {
        let tables: Tables = Tables::default();
        let b1: Board = Board::try_from(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        ).unwrap();
        let b2: Board = Board::try_from(START_FEN).unwrap();
        let m1: MoveList = b1.generate_moves(&tables);
        let m2: MoveList = b2.generate_moves(&tables);

        let m1_len = m1.len();

        println!("{}", b1);
        for m in m1 {
            println!("{}", m);
        };

        assert_eq!(48, m1_len);
        assert_eq!(20, m2.len());
    }

    #[test]
    fn test_make_move() {
        let tables: Tables = Tables::default();

        // move ignoring check is not made
        let king_check: Board = Board::try_from(
            "r3k2r/p1pPqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1"
        ).unwrap();
        let ignore_check: Move = Move::encode(
            Square::C7, Square::C5, Piece::BP, Piece::WP, Piece::WP, 0, 1, 0, 0);

        println!("\n{}\n{}", ignore_check, king_check);

        assert!(king_check.make_move(ignore_check, &tables).is_none());

        // castling rights changed accordingly when Color::White rook captures black rook
        let rook_takes_rook: Board = Board::try_from(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q2/PPPBBPP1/R3K2R w KQkq - 0 1"
        ).unwrap();
        let take_rook: Move = Move::encode(
            Square::H1, Square::H8, Piece::WR, Piece::BR, Piece::WP, 1, 0, 0, 0);

        println!("\n{}\n{}", take_rook, rook_takes_rook);

        let new: Board = rook_takes_rook.make_move(take_rook, &tables).unwrap();
        assert_eq!(new.castling_rights, CastlingRights::try_from("Qq").unwrap());
    }

    #[test]
    fn test_pin_mask() {
        let tables: Tables = Tables::default();
        let board: Board = Board::try_from(
            "R2bk3/5p2/4r1B1/1Q6/8/4Q3/8/2K5 b - - 0 1"
        ).unwrap();
        println!("{}", board);

        let (pinned, diag, hv) = board.map_pins(&tables);
        println!("{}\n{}\n{}", pinned, diag, hv);

        assert_eq!(pinned, BitBoard(1056776));
        assert_eq!(diag, BitBoard(4202496));
        assert_eq!(hv, BitBoard(17661175009295));
    }

    #[test]
    fn test_rook() {
        let tables: Tables = Tables::default();
        let board: Board = Board::try_from(
            "R2bk3/1r3p2/4r1B1/1Q6/8/4Q3/8/2K5 b - - 0 1"
        ).unwrap();

        println!("{}", board);

        let move_list = board.generate_legal(&tables);
        let length = move_list.len();
        for m in move_list { println!("{}", m) }

        assert_eq!(length, 4); // either the king or handle the queen check
    }

    #[test]
    fn test_queen() {
        let tables: Tables = Tables::default();
        let board: Board = Board::try_from(
            "2q5/8/8/2P5/2K1Q1r1/8/8/8 w - - 0 1"
        ).unwrap();
        println!("{}", board);

        let move_list = board.generate_legal(&tables);
        let length = move_list.len();
        for m in move_list { println!("{}", m) }

        panic!();
    }

    #[test]
    fn test_legal_pawn() {
        let tables: Tables = Tables::default();

        let b1: Board = Board::try_from("8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3 0 1").unwrap();
        println!("{}", b1);
        let m1 = b1.generate_legal(&tables); // enpassant blocks check
        assert_eq!(m1.len(), 6);

        let b2: Board = Board::try_from("8/8/8/2k5/3Pp3/8/8/4K3 b - d3 0 1").unwrap();
        println!("{}", b2);
        let m2 = b2.generate_legal(&tables); // enpassant captures checker
        assert_eq!(m2.len(), 9);

        let b3: Board = Board::try_from("8/8/8/8/k2Pp2Q/8/8/3K4 b - d3 0 1").unwrap();
        println!("{}", b3);
        let m3 = b3.generate_legal(&tables); // enpassant would leave the king in check
        assert_eq!(m3.len(), 6);
    }

    fn perft_d(board: &Board, tables: &Tables, depth: u8) -> u64 {
        if depth == 1 { return board.generate_legal(tables).len() as u64; }

        let move_list: MoveList = board.generate_legal(tables);
        let mut nodes: u64 = 0;
        for m in move_list {
            let new_board = board.make_m(m);
            
            nodes += perft_d(&new_board, tables, depth - 1);
        }

        nodes
    }

    use std::time::Instant;

    fn perft(board: &Board, tables: &Tables, depth: u8) {
        let move_list: MoveList = board.generate_legal(tables);
        let mut total_nodes = 0;

        let start = Instant::now();
        for m in move_list {
            let start = Instant::now();
            let root = board.make_m(m);
            let nodes = perft_d(&root, tables, depth - 1);
            total_nodes += nodes;
            let duration = start.elapsed();

            println!("{} -- {} nodes in {:?}", m, nodes, duration);
        }
        let duration = start.elapsed();

        let perf: u128 = total_nodes as u128 / duration.as_micros();
        println!("\n{} nodes in {:?} - {}Mnodes/s", total_nodes, duration, perf);
    }

    #[test]
    fn perftest() {
        let tables: Tables = Tables::default();
        let b = Board::try_from(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
        //"r3k3/p1ppqpb1/bn2Pnp1/4N3/1p2r3/P1N2Q1p/1PPBBPPP/R3K2R w KQq - 0 1"
        ).unwrap();

        println!("{}", b);
        println!("\nLEGAL MOVES: {}", b.generate_legal(&tables).len());

        perft(&b, &tables, 5);

        panic!();
    }
}

