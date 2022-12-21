/// Implements all the structures defining a square on the board:
/// 
/// Square: enum of the 64 squares with standard notation, from A8 to H1
/// File  : A B C D E F G H
/// Rank  : 8 7 6 5 4 3 2 1  (enum indexed backwards)

use std::mem::transmute;
use std::fmt;

use crate::bitboard::BitBoard;
use crate::from;

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
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
        write!(f,"{}", s)
    }
}

/// Parses fen formatted square (normal formatting).
impl TryFrom<&str> for Square {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.len() != 2 {
            return Err("Invalid square!");
        };
        
        let index = SQUARE_STR
            .iter()
            .position(|&tgt| { tgt == value })
            .ok_or("Invalid square!")?;
    
        Ok(Square::from(index))
    }
}

/// Makes a Square from first 6 bits of index.
/// Cannot incur in UB since squares are exactly 64
impl From<usize> for Square {
    fn from(index: usize) -> Self {
        unsafe { transmute((index as u8) & 63) }
    }
}

impl Square {
    /// Get square from (rank, file) coordinates
    #[inline]
    pub const fn from_coords(file: File, rank: Rank) -> Square {
        from!((rank as u8) << 3 ^ (file as u8), 63) // rank*8 + file
    }
    
    /// Flips square vertically
    #[inline]
    pub const fn flipv(self) -> Square{
        from!(self as u8 ^ 56, 63)
    }

    /// Converts square to bitboard
    #[inline]
    pub const fn to_board(self) -> BitBoard {
        BitBoard(1u64 << self as usize)
    }

    /// Gets file coordinate
    #[inline]
    pub const fn file(self) -> File {
        from!(self as u8, 7)
    }
    /// Gets rank coordinate
    #[inline]
    pub const fn rank(self) -> Rank {
        from!(self as u8 >> 3, 7)
    }
    /// Get (rank, file) coordinates of square
    #[inline]
    pub const fn coords(self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    /// Gets integer distances between current and given square
    #[inline]
    pub const fn dist(self, tgt: Square) -> (i8, i8) {
        let (tf, tr) = (tgt.file() as i8, tgt.rank() as i8);
        let (sf, sr) = (self.file() as i8, self.rank() as i8);

        (tf - sf, sr - tr)
    }

    /// Get new square from original. Wrap linear over the Square enum (H4.right() = A3)
    #[inline]
    pub const fn right(self) -> Square {
        from!(self as u8 + 1, 63)
    }

    /// Get new square from original. Wrap linear over the Square enum (A4.left() = H5)
    #[inline]
    pub const fn left(self) -> Square {
        from!((self as u8).wrapping_sub(1), 63)
    }

    /// Get new square from original. Wrap linear over the Square enum (H1.down() = H8)
    #[inline]
    pub const fn down(self) -> Square {
        from!(self as u8 + 8, 63)
    }

    /// Get new square from original. Wrap linear over the Square enum (A8.up() = A1)
    #[inline]
    pub const fn up(self) -> Square {
        from!((self as u8).wrapping_sub(8), 63)
    }
}

/// Board file enum
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum File {
    A, B, C, D, E, F, G, H,
}
use File::*;

pub const FILE_COUNT: usize = 8;
pub const ALL_FILES: [File; FILE_COUNT] = [
    A, B, C, D, E, F, G, H,
];

impl File {
    /// Gets file to the right, wraps H->A
    #[inline]
    pub const fn right(self) -> File {
        from!((self as u8) + 1, 7)
    }

    /// Gets file to the left, wraps A->H
    #[inline]
    pub const fn left(self) -> File {
        from!((self as u8).wrapping_sub(1), 7) // avoids of
    }
}

/// Board rank enum
/// Since boards are numbered A8 -> H1, ranks are backwards
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum Rank {
    Eight, Seventh, Sixth, Fifth, Fourth, Third, Second, First,
}
use Rank::*;

pub const RANK_COUNT: usize = 8;
pub const ALL_RANKS: [Rank; RANK_COUNT] = [
    Eight, Seventh, Sixth, Fifth, Fourth, Third, Second, First,
];

impl Rank {
    // Gets rank below, wraps First->Eight
    #[inline]
    pub const fn down(self) -> Rank {
        from!(self as u8 + 1, 7)
    }

    // Gets rank above, wraps Eight->First
    #[inline]
    pub const fn up(self) -> Rank {
        from!((self as u8).wrapping_sub(1), 7) // avoids of
    }
}