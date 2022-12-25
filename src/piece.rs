/// Structures to represent a piece along with its color 

use std::ops::Not;
use std::mem::transmute;
use std::fmt;

use crate::from;

/// Piece/Player color enum
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum Color { White, Black }
use Color::*;

impl Not for Color {
    type Output = Color;

    // get opposite color
    fn not(self) -> Color {
        self.opposite()
    }
}

/// Simple trick to make not trait constexpr
impl Color {
    const fn opposite(self) -> Color {
        from!(self as u8 ^ 1, 1)
    }
}

/// Pretty print color to string
impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            White => "White",
            Black => "Black",
        })
    }
}

/// Chess Piece enum (includes color)
/// Pieces alternate between Black and White so that the least significant bit is the color
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum Piece { WP, BP, WN, BN, WB, BB, WR, BR, WQ, BQ, WK, BK }
use Piece::*;

pub const PIECE_COUNT: usize = 12;
pub const ALL_PIECES: [Piece; PIECE_COUNT] = [
    WP, BP, WN, BN, WB, BB,
    WR, BR, WQ, BQ, WK, BK,
];

// used in move generation, promotions ordered by relative importance
pub const WHITE_PIECES    : [Piece; 6] = [ WP, WN, WB, WR, WQ, WK ];
pub const BLACK_PIECES    : [Piece; 6] = [ BP, BN, BB, BR, BQ, BK ];
pub const WHITE_PROMOTIONS: [Piece; 4] = [ WQ, WN, WR, WB ];
pub const BLACK_PROMOTIONS: [Piece; 4] = [ BQ, BN, BR, BB ];

// extract information from piece index
pub const PIECE_BITS: usize = 0x0E;
pub const PAWN      : usize = 0x00;
pub const KNIGHT    : usize = 0x02;
pub const BISHOP    : usize = 0x04;
pub const ROOK      : usize = 0x06;
pub const QUEEN     : usize = 0x08;
pub const KING      : usize = 0x0A;

// used for printing/reading pieces
const PIECE_CHAR: [char; PIECE_COUNT] = [
    'P', 'p', 'N', 'n', 'B', 'b',
    'R', 'r', 'Q', 'q', 'K', 'k',
];
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

/// Reads piece from uci string
impl TryFrom<char> for Piece {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        Ok(Self::from(
            PIECE_CHAR
                .iter()
                .position(|&x| x == value )
                .ok_or("Invalid piece!")?
        ))
    }
}

/// Create piece from usize index
/// 
/// UB:
/// If 12 <= index mod 16 <=15 this will try to transmute to a non-existent piece
/// Simply use indices that make sense
impl From<usize> for Piece {
    fn from(index: usize) -> Self {
        unsafe { transmute((index as u8) & 15) }
    }
}

impl Piece {
    /// Returns fen formatted piece
    pub fn to_char(self) -> char {
        PIECE_CHAR[self as usize]
    }

    /// Get piece color
    pub const fn color(self) -> Color {
        from!(self as u8, 1)
    }

    /// Switch piece color
    pub const fn opposite_color(self) -> Piece {
        from!(self as u8 ^ 1, 15) // ^1 flips color bit
    }
}