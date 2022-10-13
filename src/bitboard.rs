use core::fmt;
use std::ops::{Shl, Shr, BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Mul, Not};

use crate::square::Square;
// std::ops traits are shamelessly stolen from https://github.com/jordanbray/chess


#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct BitBoard(pub u64);

pub const EMPTY_BOARD: BitBoard = BitBoard(0);

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

            if self.get_bit(Square::from_index(square)) {
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

    /// Iterate over set squares in bitboard
    fn next(&mut self) -> Option<Square> {
        if *self == EMPTY_BOARD {
            None
        } else {        
            let index = self.pop_first();
            *self ^= index.to_board();
            Some(index)
        }
    }
}

impl BitBoard {
    pub fn new(bb: u64) -> BitBoard {
        BitBoard{ 0: bb }
    }

    #[inline]
    pub fn get_bit(&self, square: Square) -> bool {
        self.0 & (1u64 << square.index()) != 0
    }
    #[inline]
    pub fn set_bit(&mut self, square: Square) {
        self.0 |= 1u64 << square.index();
    }
    #[inline]
    pub fn pop_bit(&mut self, square: Square) {
        if self.get_bit(square) {
            self.0 ^= 1u64 << square.index();
        }
    }

    /// Returns popcnt
    /// Using RUSTFLAGS='target-cpu=native' we enforce llvm to use the popcntl simd instruction
    #[inline]
    pub fn count_bits(&self) -> u32 {
        self.0.count_ones()
    }

    /// Pops first set square from board (least significant 1 bit)
    #[inline]
    pub fn pop_first(self) -> Square {
        Square::from_index(self.0.trailing_zeros() as usize)
    }
}