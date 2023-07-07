use std::fmt;
use std::ops::Not;

use crate::transmute_enum;

/// Piece/Player color enum
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum Color {
    White,
    Black,
}

impl Not for Color {
    type Output = Color;

    // get opposite color
    fn not(self) -> Color {
        transmute_enum!(self as u8 ^ 1, 1)
    }
}

/// Pretty print color to string
impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Color::White => "White",
                Color::Black => "Black",
            }
        )
    }
}

/// Chess Piece enum (includes color)
/// Pieces alternate between Black and White so that the least significant bit is the color
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
#[rustfmt::skip]
pub enum Piece {
    WP, BP, WN, BN, WB, BB, WR, BR, WQ, BQ, WK, BK,
}
use Piece::*;

pub const PIECE_COUNT: usize = 12;

/// All pieces indexed by binary representation
#[rustfmt::skip]
pub const ALL_PIECES: [Piece; PIECE_COUNT] = [
    WP, BP, WN, BN, WB, BB,
    WR, BR, WQ, BQ, WK, BK,
];

/// All pieces indexed by color
#[rustfmt::skip]
pub const PIECES: [[Piece; 6]; 2] = [
    [ WP, WN, WB, WR, WQ, WK ],
    [ BP, BN, BB, BR, BQ, BK ]
];

// used for printing/reading pieces
#[rustfmt::skip]
const PIECE_CHAR: [char; PIECE_COUNT] = [
    'P', 'p', 'N', 'n', 'B', 'b',
    'R', 'r', 'Q', 'q', 'K', 'k',
];

#[rustfmt::skip]
const PIECE_UNICODE: [char; PIECE_COUNT] = [
    '♟', '♙', '♞', '♘', '♝', '♗',
    '♜', '♖', '♛', '♕', '♚', '♔',
];

/// Prints piece as unicode character
impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", PIECE_UNICODE[*self as usize])
    }
}

/// Reads piece from uci char
impl TryFrom<char> for Piece {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(Self::from(
            PIECE_CHAR
                .iter()
                .position(|&x| x == value)
                .ok_or("Invalid piece!")?,
        ))
    }
}

/// Implement methods to:
///  - get each piece based on color
///  - check if any piece is of a certain type
macro_rules! impl_conversions {
    ($($piece:ident, $fn:ident, $val:literal),*) => {
        $(
            impl Color {
                pub const fn $piece(self) -> Piece {
                    transmute_enum!($val + self as u8, 15)
                }
            }

            impl Piece {
                pub const fn $fn(self) -> bool {
                    self as u8 & 0b1110 == $val
                }
            }
        )*
    };
}
impl_conversions! {
    pawn,   is_pawn,   0b0000,
    knight, is_knight, 0b0010,
    bishop, is_bishop, 0b0100,
    rook,   is_rook,   0b0110,
    queen,  is_queen,  0b1000,
    king,   is_king,   0b1010
}

/// Create piece from usize index
/// UB:
/// If 12 <= index mod 16 <=15 this will try to transmute to a non-existent piece
/// Simply use indices that make sense
impl From<usize> for Piece {
    fn from(index: usize) -> Self {
        transmute_enum!(index as u8, 15)
    }
}

impl Piece {
    /// Returns fen formatted piece
    pub const fn to_char(self) -> char {
        PIECE_CHAR[self as usize]
    }

    /// Get piece color
    pub const fn color(self) -> Color {
        transmute_enum!(self as u8, 1)
    }

    /// Switch piece color
    pub const fn opposite_color(self) -> Piece {
        transmute_enum!(self as u8 ^ 1, 15) // ^1 flips color bit
    }
}
