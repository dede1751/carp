//! # Implements all the structures defining a square on the board:
//! 
//!     ## Square: enum of the 64 squares with standard notation, from A8 to H1
//!     ## File  : A B C D E F G H
//!     ## Rank  : 8 7 6 5 4 3 2 1  (enum indexed backwards)

use std::mem::transmute;
use std::fmt;

use crate::bitboard::BitBoard;

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
    pub fn from_coords(file: File, rank: Rank) -> Square {
        let index: usize = (rank as usize) << 3 ^ (file as usize); // rank*8 + file
        Square::from(index)
    }
    
    /// Flips square vertically
    #[inline]
    pub fn flipv(self) -> Square{
        Square::from(self as usize ^ 56)
    }

    /// Converts square to bitboard
    #[inline]
    pub fn to_board(self) -> BitBoard {
        BitBoard::new(1u64 << self as usize)
    }

    /// Gets file coordinate
    #[inline]
    pub fn file(self) -> File {
        File::from(self as usize)
    }
    /// Gets rank coordinate
    #[inline]
    pub fn rank(self) -> Rank {
        Rank::from(self as usize >> 3)
    }
    /// Get (rank, file) coordinates of square
    #[inline]
    pub fn coords(self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    /// Gets integer distances between current and given square
    #[inline]
    pub fn dist(self, tgt: Square) -> (i8, i8) {
        let (tf, tr) = (tgt.file() as i8, tgt.rank() as i8);
        let (sf, sr) = (self.file() as i8, self.rank() as i8);

        (tf - sf, sr - tr)
    }

    // Get new square from original. Wrap linear over the Square enum (H4.right() = A3)
    #[inline]
    pub fn right(self) -> Square {
        Square::from(self as usize + 1)
    }
    #[inline]
    pub fn left(self) -> Square {
        Square::from((self as usize).wrapping_sub(1))
    }
    #[inline]
    pub fn down(self) -> Square {
        Square::from(self as usize + 8)
    }
    #[inline]
    pub fn up(self) -> Square {
        Square::from((self as usize).wrapping_sub(8))
    }
}


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

/// Make file from usize, wrap at 7. 
impl From<usize> for File {
    fn from(index: usize) -> Self {
        unsafe { transmute((index as u8) & 7) }
    }
}

impl File {
    /// Gets file to the right, wraps H->A
    #[inline]
    pub fn right(self) -> File {
        File::from((self as usize) + 1)
    }

    /// Gets file to the left, wraps A->H
    #[inline]
    pub fn left(self) -> File {
        File::from((self as usize).wrapping_sub(1)) // avoids of
    }
}


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

/// Make rank from usize, wrap at 7. 
impl From<usize> for Rank {
    fn from(index: usize) -> Self {
        unsafe { transmute((index as u8) & 7) }
    }
}

impl Rank {
    // Gets rank below, wraps First->Eight
    #[inline]
    pub fn down(self) -> Rank {
        Rank::from((self as usize) + 1)
    }

    // Gets rank above, wraps Eight->First
    #[inline]
    pub fn up(self) -> Rank {
        Rank::from((self as usize).wrapping_sub(1)) // avoid of
    }
}