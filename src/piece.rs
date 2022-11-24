//! # Structures to represent a piece along with its color 

use std::ops::Not;
use std::mem::transmute;

/// Player color enum
#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash)]
pub enum Color { White, Black }
use Color::*;

impl Not for Color {
    type Output = Color;

    // get opposite color
    #[inline]
    fn not(self) -> Color {
        unsafe { transmute(1 ^ self as u8) }
    }
}

impl Color {
    /// Get usize index of color
    #[inline]
    pub fn index(&self) -> usize {
        *self as usize
    }

    /// Get color from usize index
    #[inline]
    pub fn from_index(index: usize) -> Color {
        unsafe { transmute((index as u8) & 1) }
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
/// 
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

// extract information from piece index
pub const PIECE_BITS: usize = 0x0E;
pub const PAWN: usize = 0x00;
pub const KNIGHT: usize = 0x02;
pub const BISHOP: usize = 0x04;
pub const ROOK: usize = 0x06;
pub const QUEEN: usize = 0x08;
pub const KING: usize = 0x0A;

// used in move generation
pub const WHITE_PIECES: [Piece; 6] = [ WP, WN, WB, WR, WQ, WK ];
pub const WHITE_PROMOTIONS: [Piece; 4] = [ WQ, WN, WR, WB ]; // order promotions by relative importance
pub const BLACK_PIECES: [Piece; 6] = [ BP, BN, BB, BR, BQ, BK ];
pub const BLACK_PROMOTIONS: [Piece; 4] = [ BQ, BN, BR, BB ];

// used for printing/reading pieces
const PIECE_CHAR: [char; PIECE_COUNT] = [
    'P', 'p', 'N', 'n', 'B', 'b',
    'R', 'r', 'Q', 'q', 'K', 'k',
];
const PIECE_UNICODE: [char; PIECE_COUNT] = [
    '♟', '♙', '♞', '♘', '♝', '♗',
    '♜', '♖', '♛', '♕', '♚', '♔',
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
        Color::from_index(1 & *self as usize)
    }

    /// Switch piece color
    #[inline]
    pub fn opposite_color(&self) -> Piece {
        Piece::from_index(self.index() ^ 1)
    }
}