//! # Implements castling related operations
//! 
use std::fmt;

use crate::{
    bitboard::BitBoard,
    square::*,
    piece::Color,
};

///     Castling rights struct
/// Implemented through a flag bit vector. This allows for fast castle update without needing
/// bitboard lookups.
/// 
///  WK | WQ | BK | BQ  --> only using least significant 8 bits
///  08   04   02   01 
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub struct CastlingRights (u8);
pub const CASTLE_COUNT: usize = 16;
pub const NO_RIGHTS: CastlingRights = CastlingRights(0);

// bit masks each right
const WK: u8 = 0x08;
const WQ: u8 = 0x04;
const BK: u8 = 0x02;
const BQ: u8 = 0x01;

// bit masks for removing castle rights
const ALL: u8 = 0x0F;
const NO_WK: u8 = ALL ^ WK;
const NO_WQ: u8 = ALL ^ WQ;
const NO_BK: u8 = ALL ^ BK;
const NO_BQ: u8 = ALL ^ BQ;
const NO_W: u8 = NO_WK & NO_WQ;
const NO_B: u8 = NO_BK & NO_BQ;

// used to update castling rights during move generation
const CASTLE_MASKS: [u8; SQUARE_COUNT] = [
    NO_BQ, ALL, ALL, ALL, NO_B, ALL, ALL, NO_BK,
      ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
      ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
      ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
      ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
      ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
      ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
    NO_WQ, ALL, ALL, ALL, NO_W, ALL, ALL, NO_WK,
];

// castle source squares
pub const CASTLE_SQUARES: [Square; 2] = [ Square::E1, Square::E8 ];

// bitboards for kingside castling relevant occupancies, 0 -> White, 1 -> Black
pub const KINGSIDE_OCCUPANCIES: [BitBoard; 2] = [
    BitBoard(6917529027641081856),
    BitBoard(96),
];
// squares skipped by the kingside castling move
pub const KINGSIDE_SQUARES: [Square; 2] = [ Square::F1, Square::F8 ];
pub const KINGSIDE_TARGETS: [Square; 2] = [ Square::G1, Square::G8 ];

// bitboards for queenside castling relevant occupancies, 0 -> White, 1 -> Black
pub const QUEENSIDE_OCCUPANCIES: [BitBoard; 2] = [
    BitBoard(1008806316530991104),
    BitBoard(14),
];
// squares skipped by queenside castling move
pub const QUEENSIDE_SQUARES: [Square; 2] = [ Square::D1, Square::D8 ];
pub const QUEENSIDE_TARGETS: [Square; 2] = [ Square::C1, Square::C8 ];

/// Prints rights to fen format
impl fmt::Display for CastlingRights {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s: String = String::from("");

        if self.0 & WK != 0 { s.push('K') };
        if self.0 & WQ != 0 { s.push('Q') };
        if self.0 & BK != 0 { s.push('k') };
        if self.0 & BQ != 0 { s.push('q') };
        if s.is_empty() {s.push('-')};

        write!(f, "{}", s)
    }
}

/// Parses fen castling string to return rights
impl TryFrom<&str> for CastlingRights {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut rights = NO_RIGHTS;
        
        if value == "-" {
            return Ok(NO_RIGHTS);
        }

        for token in value.chars(){
            match token {
                'K' => { 
                    if rights.0 & (BK | BQ) != 0 {
                        return Err("Invalid Castling Rights!");
                    }
                    rights.0 |= WK;
                },
                'Q' => {
                    if rights.0 & (BK | BQ) != 0 {
                        return Err("Invalid Castling Rights!");
                    }
                    rights.0 |= WQ;
                },
                'k' => rights.0 |= BK,
                'q' => rights.0 |= BQ,
                 _  => return Err("Invalid Castling Rights!"), 
            }
        }

        Ok(rights)
    }
}


impl CastlingRights {
    /// Get index of rights as usize
    pub fn index(&self) -> usize {
        self.0 as usize
    }

    /// Checks whether given color has kingside rights
    #[inline]
    pub fn has_kingside(&self, side: Color) -> bool {
        match side {
            Color::White => self.0 & WK != 0,
            Color::Black => self.0 & BK != 0,
        }
    }

    /// Checks whether given color has queenside rights
    #[inline]
    pub fn has_queenside(&self, side: Color) -> bool {
        match side {
            Color::White => self.0 & WQ != 0,
            Color::Black => self.0 & BQ != 0,
        }
    }

    /// Updates rights according to move.
    /// Based on the idea that any move starting or ending on one of the four corners of the board
    /// will remove the rights relative to that corner, and remove all rights in case the move 
    /// starts (or ends but it's impossible) on the king start square
    #[inline]
    pub fn update(&self, src: Square, tgt: Square) -> CastlingRights {
        let mut new: CastlingRights = *self;
        
        new.0 &= CASTLE_MASKS[src.index()];
        new.0 &= CASTLE_MASKS[tgt.index()];
        new
    }
}