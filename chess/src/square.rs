use std::{fmt, str::FromStr};

use crate::transmute_enum;
use crate::{bitboard::*, piece::Color};

/// Board square enum, indexed from A8 = 0 to H1 = 63
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
#[rustfmt::skip]
pub enum Square {
    A8, B8, C8, D8, E8, F8, G8, H8, 
    A7, B7, C7, D7, E7, F7, G7, H7, 
    A6, B6, C6, D6, E6, F6, G6, H6, 
    A5, B5, C5, D5, E5, F5, G5, H5, 
    A4, B4, C4, D4, E4, F4, G4, H4, 
    A3, B3, C3, D3, E3, F3, G3, H3, 
    A2, B2, C2, D2, E2, F2, G2, H2, 
    A1, B1, C1, D1, E1, F1, G1, H1,
}
use Square::*;

pub const SQUARE_COUNT: usize = 64;

#[rustfmt::skip]
pub const ALL_SQUARES: [Square; SQUARE_COUNT] = [
    A8, B8, C8, D8, E8, F8, G8, H8, 
    A7, B7, C7, D7, E7, F7, G7, H7, 
    A6, B6, C6, D6, E6, F6, G6, H6, 
    A5, B5, C5, D5, E5, F5, G5, H5, 
    A4, B4, C4, D4, E4, F4, G4, H4, 
    A3, B3, C3, D3, E3, F3, G3, H3, 
    A2, B2, C2, D2, E2, F2, G2, H2, 
    A1, B1, C1, D1, E1, F1, G1, H1,
];

#[rustfmt::skip]
const SQUARE_STR: [&str; SQUARE_COUNT] = [
    "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8", 
    "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7", 
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6", 
    "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5", 
    "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", 
    "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3", 
    "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2", 
    "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", 
];

/// Print fen formatted square.
impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: String = String::from(SQUARE_STR[*self as usize]);
        write!(f, "{s}")
    }
}

/// Parses fen formatted square (normal formatting).
impl FromStr for Square {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() != 2 {
            return Err("Invalid square!");
        };

        let index = SQUARE_STR
            .iter()
            .position(|&tgt| tgt == s)
            .ok_or("Invalid square!")?;

        Ok(Square::from(index))
    }
}

/// Makes a Square from first 6 bits of index.
/// Cannot incur in UB since squares are exactly 64
impl From<usize> for Square {
    fn from(index: usize) -> Self {
        transmute_enum!(index as u8, 63)
    }
}

impl Square {
    /// Get square from (rank, file) coordinates
    pub const fn from_coords(file: File, rank: Rank) -> Square {
        transmute_enum!((rank as u8) << 3 ^ (file as u8), 63) // rank*8 + file
    }

    /// Converts square to bitboard
    pub const fn to_board(self) -> BitBoard {
        BitBoard(1u64 << self as usize)
    }

    /// Gets file coordinate
    pub const fn file(self) -> File {
        transmute_enum!(self as u8, 7)
    }
    /// Gets rank coordinate
    pub const fn rank(self) -> Rank {
        transmute_enum!(self as u8 >> 3, 7)
    }
    /// Get (rank, file) coordinates of square
    pub const fn coords(self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    /// Gets integer distances between current and given square
    pub const fn dist(self, tgt: Square) -> (i8, i8) {
        let (tf, tr) = (tgt.file() as i8, tgt.rank() as i8);
        let (sf, sr) = (self.file() as i8, self.rank() as i8);

        (tf - sf, sr - tr)
    }

    /// Get new square by flipping the rank of the original.
    pub const fn flipv(self) -> Square {
        transmute_enum!(self as u8 ^ 56, 63)
    }

    /// Get new square moving forward from original based on side.
    /// To go backwards, simply use the opposite side.
    pub const fn forward(self, side: Color) -> Square {
        match side {
            Color::White => self.up(),
            Color::Black => self.down(),
        }
    }

    /// Get new square from original. Wrap linear over the Square enum (H4.right() = A3)
    pub const fn right(self) -> Square {
        transmute_enum!(self as u8 + 1, 63)
    }

    /// Get new square from original. Wrap linear over the Square enum (A4.left() = H5)
    pub const fn left(self) -> Square {
        transmute_enum!((self as u8).wrapping_sub(1), 63)
    }

    /// Get new square from original. Wrap linear over the Square enum (H1.down() = H8)
    pub const fn down(self) -> Square {
        transmute_enum!(self as u8 + 8, 63)
    }

    /// Get new square from original. Wrap linear over the Square enum (A8.up() = A1)
    pub const fn up(self) -> Square {
        transmute_enum!((self as u8).wrapping_sub(8), 63)
    }

    /// Checks if a square is on the starting rank for the given side
    pub const fn is_start_square(self, side: Color) -> bool {
        match side {
            Color::White => self.rank() as u8 == Rank::Second as u8,
            Color::Black => self.rank() as u8 == Rank::Seventh as u8,
        }
    }

    /// Checks if a square is on the promoting rank for the given side
    pub const fn is_promotion_square(self, side: Color) -> bool {
        match side {
            Color::White => self.rank() as u8 == Rank::Eight as u8,
            Color::Black => self.rank() as u8 == Rank::First as u8,
        }
    }
}

/// Board file enum, indexed from A = 0 to H = 7
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
#[rustfmt::skip]
pub enum File {
    A, B, C, D, E, F, G, H,
}
use File::*;

pub const FILE_COUNT: usize = 8;

#[rustfmt::skip]
pub const ALL_FILES: [File; FILE_COUNT] = [
    A, B, C, D, E, F, G, H,
];
const FILE_CHAR: [char; FILE_COUNT] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];

impl File {
    /// Gets file to the right, wraps H->A
    pub const fn right(self) -> File {
        transmute_enum!((self as u8) + 1, 7)
    }

    /// Gets file to the left, wraps A->H
    pub const fn left(self) -> File {
        transmute_enum!((self as u8).wrapping_sub(1), 7)
    }

    /// Converts file to char
    pub const fn to_char(self) -> char {
        FILE_CHAR[self as usize]
    }
}

/// Board rank enum, indexed from Eight = 0 to First = 7 (backwards)
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
#[rustfmt::skip]
pub enum Rank {
    Eight, Seventh, Sixth, Fifth, Fourth, Third, Second, First,
}
use Rank::*;

pub const RANK_COUNT: usize = 8;

#[rustfmt::skip]
pub const ALL_RANKS: [Rank; RANK_COUNT] = [
    Eight, Seventh, Sixth, Fifth, Fourth, Third, Second, First,
];
const RANK_CHAR: [char; RANK_COUNT] = ['8', '7', '6', '5', '4', '3', '2', '1'];

impl Rank {
    /// Gets rank below, wraps First->Eight
    pub const fn down(self) -> Rank {
        transmute_enum!(self as u8 + 1, 7)
    }

    /// Gets rank above, wraps Eight->First
    pub const fn up(self) -> Rank {
        transmute_enum!((self as u8).wrapping_sub(1), 7)
    }

    /// Converts rank to a char
    pub const fn to_char(self) -> char {
        RANK_CHAR[self as usize]
    }
}
