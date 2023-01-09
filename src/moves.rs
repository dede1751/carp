/// Implements move encoding and MoveList struct

use std::{
    fmt,
    collections::HashSet
};

use crate::{
    square::{ *, Square::* },
    piece::*,
    board::Board,
    tables::Tables,
};

/// Indexed by color and file. Target squares for pawn single pushes
pub const PUSH: [[Square; SQUARE_COUNT]; 2] = [[
    A8, A8, A8, A8, A8, A8, A8, A8,
    A8, B8, C8, D8, E8, F8, G8, H8, 
    A7, B7, C7, D7, E7, F7, G7, H7, 
    A6, B6, C6, D6, E6, F6, G6, H6, 
    A5, B5, C5, D5, E5, F5, G5, H5, 
    A4, B4, C4, D4, E4, F4, G4, H4, 
    A3, B3, C3, D3, E3, F3, G3, H3, 
    A8, A8, A8, A8, A8, A8, A8, A8,], [
        
    A8, A8, A8, A8, A8, A8, A8, A8,
    A6, B6, C6, D6, E6, F6, G6, H6, 
    A5, B5, C5, D5, E5, F5, G5, H5, 
    A4, B4, C4, D4, E4, F4, G4, H4, 
    A3, B3, C3, D3, E3, F3, G3, H3, 
    A2, B2, C2, D2, E2, F2, G2, H2, 
    A1, B1, C1, D1, E1, F1, G1, H1, 
    A8, A8, A8, A8, A8, A8, A8, A8,
]];

/// Indexed by color and file. Target squares for pawn double pushes
pub const DOUBLE_PUSH: [[Square; FILE_COUNT]; 2] = [
    [ A4, B4, C4, D4, E4, F4, G4, H4 ],
    [ A5, B5, C5, D5, E5, F5, G5, H5 ]
];

/// Indexed by color, rank at which each side's pawns promote
pub const PROMOTION_RANKS: [Rank; 2] = [Rank::Seventh, Rank::Second];

/// Indexed by color, rank at which each side's pawns start
pub const START_RANKS: [Rank; 2] = [Rank::Second, Rank::Seventh];

/// Move -- Encodes move in a single 4B word
/// Smaller encoding can be achieved using only 2B (6b src, 6b tgt, 3b promotion, 1b extra) but it
/// makes the rest of the engine much less ergonomic. We only use the least significant 28 bits
/// 
///     0000 0000 0000 0000 0000 0011 1111    source             0x00003F     0
///     0000 0000 0000 0000 1111 1100 0000    target             0x000FC0     6
///     0000 0000 0000 1111 0000 0000 0000    piece              0x00F000    12
///     0000 0000 1111 0000 0000 0000 0000    captured piece     0x0F0000    16
///     0000 1111 0000 0000 0000 0000 0000    promoted piece     0x0F0000    20
///     0001 0000 0000 0000 0000 0000 0000    capture flag       0x100000    24
///     0010 0000 0000 0000 0000 0000 0000    double push flag   0x200000    25
///     0100 0000 0000 0000 0000 0000 0000    enpassant flag     0x400000    26
///     1000 0000 0000 0000 0000 0000 0000    castling flag      0x800000    27
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct Move (pub u32);

pub const NULL_MOVE: Move = Move(0);

// bit masks for the various parts of the move
const SRC        : u32 = 0x0000003F;
const TGT        : u32 = 0x00000FC0;
const PIECE      : u32 = 0x0000F000;
const CAPTURE    : u32 = 0x000F0000;
const PROMOTE    : u32 = 0x00F00000;
const IS_CAP     : u32 = 0x01000000;
const DOUBLE     : u32 = 0x02000000;
const ENPASSANT  : u32 = 0x04000000;
const CASTLE     : u32 = 0x08000000;

/// Prints move in uci format
impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = format!("{}{}", self.get_src(), self.get_tgt());

        if self.get_promotion() != Piece::WP {
            write!(f, "{}{}", s, self.get_promotion().to_char().to_ascii_lowercase())
        } else {
            write!(f,"{}", s)
        }
    }
}

impl Move {
    /// Init move through bitwise or of the various values shifted to correct place
    /// 
    /// When not promoting, pass WP
    /// Flags are u32 boolean values (must be 0 or 1)
    pub const fn encode(
        src: Square,
        tgt: Square,
        piece: Piece,
        capture: Piece,
        promote: Piece,
        is_capture: u32,
        double_push: u32,
        en_passant: u32,
        castle: u32
    ) -> Move {
        Move(
            (src as u32)            |
            (tgt as u32) << 6       |
            (piece as u32) << 12    |
            (capture as u32) << 16  |
            (promote as u32) << 20  |
            is_capture << 24        |
            double_push << 25       |
            en_passant << 26        |
            castle << 27
        )
    }

    /// Returns the move source square
    #[inline]
    pub fn get_src(&self) -> Square {
        Square::from((self.0 & SRC) as usize)
    }

    /// Returns the move target square
    #[inline]
    pub fn get_tgt(&self) -> Square {
        Square::from(((self.0 & TGT) >> 6) as usize)
    }

    /// Returns the moving piece
    #[inline]
    pub fn get_piece(&self) -> Piece {
        Piece::from(((self.0 & PIECE) >> 12) as usize)
    }

    /// Returns the piece the pawn is promoting to
    #[inline]
    pub fn get_capture(&self) -> Piece {
        Piece::from(((self.0 & CAPTURE) >> 16) as usize)
    }

    /// Returns the piece the pawn is promoting to
    #[inline]
    pub fn get_promotion(&self) -> Piece {
        Piece::from(((self.0 & PROMOTE) >> 20) as usize)
    }

    /// Returns true if the move is a promotion
    pub const fn is_promotion(&self) -> bool {
        self.0 & PROMOTE != 0
    }

    /// Returns true if the move is a capture
    pub const fn is_capture(&self) -> bool {
        self.0 & IS_CAP != 0
    }

    /// Returns true if the move is a double pawn push
    pub const fn is_double_push(&self) -> bool {
        self.0 & DOUBLE != 0
    }

    /// Returns true if the move is an enpassant capture
    pub const fn is_enpassant(&self) -> bool {
        self.0 & ENPASSANT != 0
    }

    /// Returns true if the move is a castling move
    pub const fn is_castle(&self) -> bool {
        self.0 & CASTLE != 0
    }

    /// Writes move in standard algebraic notation
    pub fn to_algebraic(&self, board: &Board, tables: &Tables) -> String {
        if self.is_castle() {
            let file = self.get_tgt().file();

            if file == File::G { return String::from("O-O"); }
            else { return String::from("O-O-O"); }
        }

        let mut move_str = String::new();
        let src = self.get_src();
        let tgt = self.get_tgt();
        let p = self.get_piece();
        let is_pawn = p == Piece::WP || p == Piece::BP;

        // disambiguate non-pawn pieces
        if !is_pawn {
            move_str.push(p.to_char().to_ascii_uppercase());

            // get all ambiguous (file, rank) source squares
            let (files, ranks): (HashSet<File>, HashSet<Rank>) = board.generate_moves(tables)
                .into_iter()
                .filter(|m|
                    m.get_src() != src  && // different source square
                    m.get_tgt() == tgt  && // same target square
                    m.get_piece() == p)    // same piece
                .map(|m| m.get_src().coords())
                .unzip();
            
            // if a move is unambiguous, files and ranks will be empty
            if files.len() != 0 && ranks.len() != 0 {
                if !files.contains(&src.file()) {
                    move_str.push(src.file().to_char())
                } else if !ranks.contains(&src.rank()) {
                    move_str.push(src.rank().to_char())
                } else {
                    move_str.push_str(&src.to_string());
                }
            }
        }

        if self.is_capture() {
            if is_pawn { move_str.push(src.file().to_char()) } // pawn captures always specify file
            move_str.push('x')
        }
        move_str.push_str(&tgt.to_string());        // destination

        // add promotion
        if self.is_promotion() {
            move_str.push(self.get_promotion().to_char())
        }

        // check/checkmate
        let new = board.make_move(*self);
        if new.king_in_check(tables) {             
            if new.generate_moves(tables).len() == 0 { move_str.push('#'); } // checkmate
            else { move_str.push('+'); } // check
        }

        move_str
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_constructor(){
        let m1: Move = Move::encode(Square::E2, Square::E4, Piece::WP, Piece::WP, Piece::WP, 0, 1, 0, 0);
        let m2: Move = Move::encode(Square::H7, Square::G8, Piece::WP, Piece::BR, Piece::WQ, 1, 0, 0, 0);

        assert!(m1.is_double_push());
        assert_eq!(m1.get_src(), Square::E2);
        assert_eq!(m1.get_tgt(), Square::E4);
        assert!(m2.is_capture());
        assert_eq!(m2.get_capture(), Piece::BR);
        assert_eq!(m2.get_promotion(), Piece::WQ);
    }
}