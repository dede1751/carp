//! # Implements Bitboards along with all relevant bitwise operations

use std::fmt;
use std::ops::{
    Shl, Shr, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Mul, Not
};
// std::ops traits are shamelessly stolen from https://github.com/jordanbray/chess

use crate::square::*;

/// Bitboard implemented as a simple tuple struct.
/// Contents are public for convenience (direct initialization in various arrays/tests) but it
/// would make no difference to using the implemented traits.
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct BitBoard(pub u64);
pub type BB64 = [BitBoard; SQUARE_COUNT];

pub const EMPTY_BB: BitBoard = BitBoard(0);
pub const EMPTY_BB64: BB64 = [EMPTY_BB; SQUARE_COUNT];

// Impl Shl
impl Shl for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn shl(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 << other.0)
    }
}

impl Shl for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn shl(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 << other.0)
    }
}

impl Shl<&BitBoard> for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn shl(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 << other.0)
    }
}

impl Shl<BitBoard> for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn shl(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 << other.0)
    }
}
// Impl Shr
impl Shr for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn shr(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 >> other.0)
    }
}

impl Shr for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn shr(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 >> other.0)
    }
}

impl Shr<&BitBoard> for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn shr(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 >> other.0)
    }
}

impl Shr<BitBoard> for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn shr(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 >> other.0)
    }
}

// Impl BitAnd
impl BitAnd for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitand(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 & other.0)
    }
}

impl BitAnd for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitand(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 & other.0)
    }
}

impl BitAnd<&BitBoard> for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitand(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 & other.0)
    }
}

impl BitAnd<BitBoard> for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitand(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 & other.0)
    }
}

// Impl BitOr
impl BitOr for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitor(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 | other.0)
    }
}

impl BitOr for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitor(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 | other.0)
    }
}

impl BitOr<&BitBoard> for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitor(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 | other.0)
    }
}

impl BitOr<BitBoard> for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitor(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 | other.0)
    }
}

// Impl BitXor

impl BitXor for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitxor(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 ^ other.0)
    }
}

impl BitXor for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitxor(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 ^ other.0)
    }
}

impl BitXor<&BitBoard> for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitxor(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0 ^ other.0)
    }
}

impl BitXor<BitBoard> for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn bitxor(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0 ^ other.0)
    }
}

// Impl BitAndAssign

impl BitAndAssign for BitBoard {
    #[inline]
    fn bitand_assign(&mut self, other: BitBoard) {
        self.0 &= other.0;
    }
}

impl BitAndAssign<&BitBoard> for BitBoard {
    #[inline]
    fn bitand_assign(&mut self, other: &BitBoard) {
        self.0 &= other.0;
    }
}

// Impl BitOrAssign
impl BitOrAssign for BitBoard {
    #[inline]
    fn bitor_assign(&mut self, other: BitBoard) {
        self.0 |= other.0;
    }
}

impl BitOrAssign<&BitBoard> for BitBoard {
    #[inline]
    fn bitor_assign(&mut self, other: &BitBoard) {
        self.0 |= other.0;
    }
}

// Impl BitXor Assign
impl BitXorAssign for BitBoard {
    #[inline]
    fn bitxor_assign(&mut self, other: BitBoard) {
        self.0 ^= other.0;
    }
}

impl BitXorAssign<&BitBoard> for BitBoard {
    #[inline]
    fn bitxor_assign(&mut self, other: &BitBoard) {
        self.0 ^= other.0;
    }
}

// Impl Mul
impl Mul for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn mul(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0.wrapping_mul(other.0))
    }
}

impl Mul for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn mul(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0.wrapping_mul(other.0))
    }
}

impl Mul<&BitBoard> for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn mul(self, other: &BitBoard) -> BitBoard {
        BitBoard(self.0.wrapping_mul(other.0))
    }
}

impl Mul<BitBoard> for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn mul(self, other: BitBoard) -> BitBoard {
        BitBoard(self.0.wrapping_mul(other.0))
    }
}

// Impl Not
impl Not for BitBoard {
    type Output = BitBoard;

    #[inline]
    fn not(self) -> BitBoard {
        BitBoard(!self.0)
    }
}

impl Not for &BitBoard {
    type Output = BitBoard;

    #[inline]
    fn not(self) -> BitBoard {
        BitBoard(!self.0)
    }
}

// Impl Display
impl fmt::Display for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = format!("\n      Bitboard: {}\n", self.0);

        for square in 0..64 {
            if square % 8 == 0 {
                s.push_str(format!("\n{}   ", (8 - square / 8)).as_str())
            }

            if self.get_bit(Square::from(square)) {
                s.push_str("X ");
            } else {
                s.push_str("- ");
            }
        }
        s.push_str("\n\n    A B C D E F G H");
        write!(f, "{}", s)
    }
}

// Impl Iterator (over squares)
impl Iterator for BitBoard {
    type Item = Square;

    // Iterate over set squares in bitboard
    fn next(&mut self) -> Option<Square> {
        if *self == EMPTY_BB {
            None
        } else {        
            let index = self.ls1b_index(); // get ls1b index
            *self ^= index.to_board();             // pops bit at ls1b
            Some(index)
        }
    }
}

impl BitBoard {
    pub fn new(bb: u64) -> BitBoard {
        BitBoard{ 0: bb }
    }

    /// Check whether given square is set on the board
    #[inline]
    pub fn get_bit(self, square: Square) -> bool {
        self.0 & (1u64 << square as usize) != 0
    }

    /// Sets given square on the board
    #[inline]
    pub fn set_bit(&mut self, square: Square) {
        self.0 |= 1u64 << square as usize;
    }

    /// Pops given square off the board
    #[inline]
    pub fn pop_bit(&mut self, square: Square) {
        if self.get_bit(square) {
            self.0 ^= 1u64 << square as usize;
        }
    }

    /// Returns popcnt
    /// Using RUSTFLAGS='target-cpu=native' we enforce llvm to use the popcntl simd instruction
    #[inline]
    pub fn count_bits(self) -> u32 {
        self.0.count_ones()
    }

    /// Pops first set square from board (least significant 1 bit)
    #[inline]
    pub fn ls1b_index(self) -> Square {
        Square::from(self.0.trailing_zeros() as usize)
    }
}