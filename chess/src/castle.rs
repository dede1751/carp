use std::{fmt, str::FromStr};

use crate::{piece::*, square::*};

/// Castling rights struct
/// Implemented through a flag bit vector. This allows for fast castle update without needing
/// bitboard lookups.
///
///  WK | WQ | BK | BQ  --> only using least significant 8 bits
///  08   04   02   01
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub struct CastlingRights(u8);

// bit masks for each right
const WK: u8 = 0x08;
const WQ: u8 = 0x04;
const BK: u8 = 0x02;
const BQ: u8 = 0x01;

const KINGSIDE: [u8; 2] = [WK, BK];
const QUEENSIDE: [u8; 2] = [WQ, BQ];

// bit masks for removing castle rights
const ALL: u8 = 0x0F;
const NO_WK: u8 = ALL ^ WK;
const NO_WQ: u8 = ALL ^ WQ;
const NO_BK: u8 = ALL ^ BK;
const NO_BQ: u8 = ALL ^ BQ;
const NO_W: u8 = NO_WK & NO_WQ;
const NO_B: u8 = NO_BK & NO_BQ;

/// Returns the rook src/tgt square for a given king target square
/// King target square must be a valid castling destination, so either C1/C8 or G1/G8
pub const fn rook_castling_move(king_tgt: Square) -> (Square, Square) {
    match king_tgt.file() {
        File::C => (king_tgt.left().left(), king_tgt.right()),
        File::G => (king_tgt.right(), king_tgt.left()),
        _ => unreachable!(),
    }
}

/// Prints rights to fen format
impl fmt::Display for CastlingRights {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s: String = String::from("");

        if self.0 & WK != 0 {
            s.push('K')
        };
        if self.0 & WQ != 0 {
            s.push('Q')
        };
        if self.0 & BK != 0 {
            s.push('k')
        };
        if self.0 & BQ != 0 {
            s.push('q')
        };
        if s.is_empty() {
            s.push('-')
        };

        write!(f, "{s}")
    }
}

/// Parses fen castling string to return rights
impl FromStr for CastlingRights {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut rights = Self::NONE;

        if s == "-" {
            return Ok(Self::NONE);
        }

        for token in s.chars() {
            match token {
                'K' => {
                    if rights.0 & (BK | BQ) != 0 {
                        return Err("Invalid Castling Rights!");
                    }
                    rights.0 |= WK;
                }
                'Q' => {
                    if rights.0 & (BK | BQ) != 0 {
                        return Err("Invalid Castling Rights!");
                    }
                    rights.0 |= WQ;
                }
                'k' => rights.0 |= BK,
                'q' => rights.0 |= BQ,
                _ => return Err("Invalid Castling Rights!"),
            }
        }

        Ok(rights)
    }
}

impl CastlingRights {
    pub const COUNT: usize = 16;
    pub const NONE: CastlingRights = CastlingRights(0);

    /// Get index of rights as usize
    pub const fn index(self) -> usize {
        self.0 as usize
    }

    /// Checks whether given color has kingside rights
    pub const fn has_kingside(self, side: Color) -> bool {
        self.0 & KINGSIDE[side as usize] != 0
    }

    /// Checks whether given color has queenside rights
    pub const fn has_queenside(self, side: Color) -> bool {
        self.0 & QUEENSIDE[side as usize] != 0
    }

    /// Updates rights according to move.
    /// Based on the idea that any move starting or ending on one of the four corners of the board
    /// will remove the rights relative to that corner, and remove all rights in case the move
    /// starts (or ends but it's impossible) on the king start square
    pub const fn update(self, src: Square, tgt: Square) -> CastlingRights {
        #[rustfmt::skip]
        const CASTLE_MASKS: [u8; Square::COUNT] = [
            NO_BQ, ALL, ALL, ALL, NO_B, ALL, ALL, NO_BK,
            ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
            ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
            ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
            ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
            ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
            ALL, ALL, ALL, ALL, ALL, ALL, ALL, ALL,
            NO_WQ, ALL, ALL, ALL, NO_W, ALL, ALL, NO_WK,
        ];

        let new = self.0 & CASTLE_MASKS[src as usize] & CASTLE_MASKS[tgt as usize];
        CastlingRights(new)
    }
}
