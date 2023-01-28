use crate::from;
use crate::square::*;
use std::fmt;

/// Bitboard implemented as a simple tuple struct.
/// Contents are public for convenience (direct initialization in various arrays/tests) but it
/// would make no difference to using the implemented traits.
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct BitBoard(pub u64);

pub type BB64 = [BitBoard; SQUARE_COUNT];

pub const EMPTY_BB: BitBoard = BitBoard(0);
pub const EMPTY_BB64: BB64 = [EMPTY_BB; SQUARE_COUNT];

/// Idea for ops implementation is from https://github.com/analog-hors/tantabus
/// Implement math standard operations
macro_rules! impl_math_ops {
    ($($trait:ident::$fn:ident),*) => {
        $(impl std::ops::$trait for BitBoard {
            type Output = Self;

            fn $fn(self, other: Self) -> Self::Output {
                Self(std::ops::$trait::$fn(self.0, other.0))
            }
        })*
    };
}

impl_math_ops! {
    Shl::shl,
    Shr::shr,
    BitAnd::bitand,
    BitOr::bitor,
    BitXor::bitxor
}

/// Mul uses different implementation for overflow protection
impl std::ops::Mul for BitBoard {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        Self(self.0.wrapping_mul(other.0))
    }
}

/// Implement math assignment operations
macro_rules! impl_math_assign_ops {
    ($($trait:ident::$fn:ident),*) => {
        $(impl std::ops::$trait for BitBoard {

            fn $fn(&mut self, other: Self) {
                std::ops::$trait::$fn(&mut self.0, other.0)
            }
        })*
    };
}

impl_math_assign_ops! {
    BitAndAssign::bitand_assign,
    BitOrAssign::bitor_assign,
    BitXorAssign::bitxor_assign
}

// Impl Not
impl std::ops::Not for BitBoard {
    type Output = BitBoard;

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

/// Impl Iterator over the 1 bits of the board
impl Iterator for BitBoard {
    type Item = Square;

    fn next(&mut self) -> Option<Square> {
        if *self == EMPTY_BB {
            None
        } else {
            let sq = self.lsb();
            self.0 &= self.0 - 1; // pops lsb

            Some(sq)
        }
    }
}

impl BitBoard {
    pub const fn new(bb: u64) -> BitBoard {
        BitBoard { 0: bb }
    }

    /// Check whether given square is set on the board
    pub const fn get_bit(self, square: Square) -> bool {
        self.0 & (1u64 << square as usize) != 0
    }

    /// Sets given square on the board
    pub const fn set_bit(self, square: Square) -> BitBoard {
        BitBoard(self.0 | 1u64 << square as usize)
    }

    /// Pops given square off the board
    pub const fn pop_bit(self, square: Square) -> BitBoard {
        BitBoard(self.0 & !(1u64 << square as usize))
    }

    /// Returns popcnt
    /// Using RUSTFLAGS='target-cpu=native' we enforce the popcnt feature
    pub const fn count_bits(self) -> u32 {
        self.0.count_ones()
    }

    /// Returns first set square from board (least significant 1 bit)
    pub const fn lsb(self) -> Square {
        from!(self.0.trailing_zeros() as u8, 63)
    }
}
