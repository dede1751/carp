mod gen;
mod magics;
mod make_move;
mod perft;

use std::{fmt, str::FromStr};

pub use crate::board::{gen::*, make_move::*, perft::*};
use crate::{
    bitboard::BitBoard,
    castle::CastlingRights,
    moves::{Move, MoveType},
    piece::{Color, Piece},
    square::{File, Rank, Square},
    zobrist::ZHash,
};

/// Bitboard-based board representation
/// Any board without a king for each player (and with more than one for either) is UB!
#[derive(Clone, Debug)]
pub struct Board {
    // Main bitboards
    pub piece_bb: [BitBoard; Piece::COUNT],
    pub side_bb: [BitBoard; 2],

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

    /// Set the piece on the board at the given square (remove first, set later)
    pub(crate) fn set_piece(&mut self, piece: Piece, square: Square) {
        let p = piece.index();
        let c = piece.color() as usize;

        self.piece_bb[p] = self.piece_bb[p].set_bit(square);
        self.side_bb[c] = self.side_bb[c].set_bit(square);
        self.piece[square as usize] = Some(piece);
        self.hash.toggle_piece(piece, square);
    }

    /// Remove the piece at the given square on the board (set first, remove later)
    /// The piece must exist at the given square
    pub(crate) fn remove_piece(&mut self, square: Square) {
        let piece = self.piece_at(square);
        let p = piece.index();
        let c = piece.color() as usize;

        self.piece_bb[p] = self.piece_bb[p].pop_bit(square);
        self.side_bb[c] = self.side_bb[c].pop_bit(square);
        self.piece[square as usize] = None;
        self.hash.toggle_piece(piece, square);
    }

    /// Looks for which piece is on the given Square
    /// Panics if no piece is on that square
    pub fn piece_at(&self, square: Square) -> Piece {
        self.piece[square as usize].unwrap()
    }

    /// Returns the piece being captured by the move.
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
    pub(crate) fn checkers(&self) -> BitBoard {
        self.attackers(self.own_king().lsb(), self.occupancy())
    }

    /// Returns true if the square is attacked by at least one enemy piece
    pub(crate) fn square_attacked(&self, square: Square, blockers: BitBoard) -> bool {
        self.attackers(square, blockers) != BitBoard::EMPTY
    }

    /// Finds legal move in board from the uci-formatted move string
    pub fn find_move(&self, move_str: &str) -> Option<Move> {
        self.gen_moves::<QUIETS>()
            .moves
            .into_iter()
            .find(|m| m.to_string() == move_str)
    }

    /// Returns bitboard with all pieces attacking a square
    pub fn map_all_attackers(&self, square: Square, blockers: BitBoard) -> BitBoard {
        self.pawns() & self.white() & pawn_attacks(square, Color::Black)
            | self.pawns() & self.black() & pawn_attacks(square, Color::White)
            | self.knights() & knight_attacks(square)
            | (self.bishops() | self.queens()) & bishop_attacks(square, blockers)
            | (self.rooks() | self.queens()) & rook_attacks(square, blockers)
            | self.kings() & king_attacks(square)
    }

    /// Returns the least valuable of the attackers within the attacker map
    pub fn get_lva(&self, attackers: BitBoard, side: Color) -> Option<(Square, Piece)> {
        let side_bb = self.side_bb[side as usize];

        for piece in Piece::SPLIT_COLOR[side as usize] {
            let squares = attackers & self.piece_bb[piece.index()] & side_bb;

            if squares != BitBoard::EMPTY {
                return Some((squares.lsb(), piece));
            }
        }

        None
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
}
