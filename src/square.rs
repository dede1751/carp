use std::mem::transmute;
use std::fmt;

use crate::bitboard::BitBoard;

/*
Square strings:

"a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8", 
"a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7", 
"a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6", 
"a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5", 
"a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", 
"a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3", 
"a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2", 
"a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1", 

A8, B8, C8, D8, E8, F8, G8, H8, 
A7, B7, C7, D7, E7, F7, G7, H7, 
A6, B6, C6, D6, E6, F6, G6, H6, 
A5, B5, C5, D5, E5, F5, G5, H5, 
A4, B4, C4, D4, E4, F4, G4, H4, 
A3, B3, C3, D3, E3, F3, G3, H3, 
A2, B2, C2, D2, E2, F2, G2, H2, 
A1, B1, C1, D1, E1, F1, G1, H1,
*/

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
pub use Square::*;

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

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s: String = String::from("");
        s.push_str(SQUARE_STR[self.index()]);
        
        write!(f,"{}", s)
    }
}

impl Square {
    // Make square from first 6 bits of index
    #[inline]
    pub fn from_index(index: usize) -> Square {
        unsafe { transmute((index as u8) & 63) }
    }
    
    // Get index of square as usize
    #[inline]
    pub fn index(&self) -> usize {
        *self as usize
    }
    
    #[inline]
    pub fn from_coords(file: File, rank: Rank) -> Square {
        let index: usize = rank.index() << 3 ^ file.index(); // rank*8 + file
        Square::from_index(index)
    }
    
    #[inline]
    pub fn to_board(&self) -> BitBoard {
        BitBoard(1u64 << self.index())
    }

    #[inline]
    pub fn file(&self) -> File {
        File::from_index(self.index() as usize)
    }
    #[inline]
    pub fn rank(&self) -> Rank {
        Rank::from_index(self.index() >> 3 as usize)
    }

    #[inline]
    pub fn dist(&self, tgt: &Square) -> (i8, i8) {
        let (tf, tr) = (tgt.file().index() as i8, tgt.rank().index() as i8);
        let (sf, sr) = (self.file().index() as i8, self.rank().index() as i8);

        (tf - sf, sr - tr)
    }

    /// Get new square from original. Wrap linear over the Square enum (H4.right() = A3)
    #[inline]
    pub fn right(&self) -> Square {
        Square::from_index(self.index() + 1)
    }
    #[inline]
    pub fn left(&self) -> Square {
        Square::from_index(self.index().wrapping_sub(1))
    }
    #[inline]
    pub fn down(&self) -> Square {
        Square::from_index(self.index() + 8)
    }
    #[inline]
    pub fn up(&self) -> Square {
        Square::from_index(self.index().wrapping_sub(8))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum File {
    A, B, C, D, E, F, G, H,
}
pub use File::*;

pub const FILE_COUNT: usize = 8;
pub const ALL_FILES: [File; FILE_COUNT] = [
    A, B, C, D, E, F, G, H,
];

impl File {
    // Make file from usize, wrap at 7. 
    #[inline]
    pub fn from_index(index: usize) -> File {
        unsafe { transmute((index as u8) & 7) }
    }

    // Get index of file as usize
    #[inline]
    pub fn index(&self) -> usize {
        *self as usize
    }

    /// Indexes are backwards!!
    // wraps to the right
    #[inline]
    pub fn right(&self) -> File {
        File::from_index(self.index() + 1)
    }

    // wraps to the left
    #[inline]
    pub fn left(&self) -> File {
        File::from_index(self.index().wrapping_sub(1)) // avoids of
    }
}


/// Since boards are numbered A8 -> H1, ranks are backwards
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum Rank {
    Eight, Seven, Six, Five, Four, Three, Two, One,
}
pub use Rank::*;

pub const RANK_COUNT: usize = 8;
pub const ALL_RANKS: [Rank; RANK_COUNT] = [
    Eight, Seven, Six, Five, Four, Three, Two, One,
];

impl Rank {
    // Make rank from usize, wrap at 7. 
    #[inline]
    pub fn from_index(index: usize) -> Rank {
        unsafe { transmute((index as u8) & 7) }
    }

    // Get index of rank as usize
    #[inline]
    pub fn index(&self) -> usize {
        *self as usize
    }

    // wraps to the top
    #[inline]
    pub fn down(&self) -> Rank {
        Rank::from_index(self.index() + 1)
    }

    // wraps to the bottom
    #[inline]
    pub fn up(&self) -> Rank {
        Rank::from_index(self.index().wrapping_sub(1)) // avoid of
    }
}