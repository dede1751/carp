//! # Structures to represent a piece along with its color 

use std::ops::Not;
use std::mem::transmute;

/// Player color enum
#[derive(Copy, Clone, Debug, Hash)]
pub enum Color { White, Black }
use Color::*;

impl Not for Color {
    type Output = Color;

    // get opposite color
    #[inline]
    fn not(self) -> Color {
        match self {
            Black => White,
            White => Black,
        }
    }
}

impl Color {
    /// Get usize index of color
    #[inline]
    pub fn index(&self) -> usize {
        match self {
            White => 0,
            Black => 1,
        }
    }
    /// Pretty print color to string
    pub fn to_string(&self) -> String {
        String::from(match self {
            White => "White",
            Black => "Black",
        })
    }

}

/// Chess Piece enum (includes color)
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum Piece { WP, WN, WB, WR, WQ, WK, BP, BN, BB, BR, BQ, BK }
use Piece::*;

pub const PIECE_COUNT: usize = 12;
pub const ALL_PIECES: [Piece; PIECE_COUNT] = [
    WP, WN, WB, WR, WQ, WK,
    BP, BN, BB, BR, BQ, BK
];

// used in move generation
pub const WHITE_PIECES: [Piece; 6] = [ WP, WN, WB, WR, WQ, WK ];
pub const WHITE_PROMOTIONS: [Piece; 4] = [ WQ, WN, WR, WB ]; // order promotions by relative importance
pub const BLACK_PIECES: [Piece; 6] = [ BP, BN, BB, BR, BQ, BK ];
pub const BLACK_PROMOTIONS: [Piece; 4] = [ BQ, BN, BR, BB ];

// used for printing/reading pieces
const PIECE_CHAR: [char; PIECE_COUNT] = [
    'P', 'N', 'B', 'R', 'Q', 'K',
    'p', 'n', 'b', 'r', 'q', 'k',
];
const PIECE_UNICODE: [char; PIECE_COUNT] = [
    '♟', '♞', '♝', '♜', '♛', '♚',
    '♙', '♘', '♗', '♖', '♕', '♔',
];


impl Piece {
    /// Get index of piece as usize
    #[inline]
    pub fn index(&self) -> usize {
        *self as usize
    }

    /// Create piece from usize index
    /// # UB 
    /// If 12 <= index mod 16 <=15 this will try to transmute to a non-existent piece
    /// Simply use indices that make sense
    pub fn from_index(index: usize) -> Piece {
        unsafe { transmute((index as u8) & 15) }
    }

    /// Create piece from fen formatted char.
    ///     None if char was not valid piece
    pub fn from_char(c: char) -> Option<Piece> {
        Some(Self::from_index(
            PIECE_CHAR
                .iter()
                .position(|&x| x == c )?
        ))
    }

    /// Returns fen formatted piece
    pub fn to_char(&self) -> char {
        PIECE_CHAR[self.index()]
    }
    /// Returns pretty-printed piece using unicode characters
    pub fn to_unicode(&self) -> char {
        PIECE_UNICODE[self.index()]
    }

    /// Get piece color
    #[inline]
    pub fn color(&self) -> Color {
        match self.index() {
            0..=5 => White,
            _     => Black,
        }
    }
}