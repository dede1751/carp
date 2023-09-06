use std::fmt;

use crate::{piece::Color, square::Square, transmute_enum};

/// Bitboard implemented as a simple tuple struct.
/// Contents are public for convenience (direct initialization in various arrays/tests) but it
/// would make no difference to using the implemented traits.
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct BitBoard(pub u64);

pub type BB64 = [BitBoard; Square::COUNT];

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
    BitAnd::bitand,
    BitOr::bitor,
    BitXor::bitxor
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

impl std::ops::Not for BitBoard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

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
        write!(f, "{s}")
    }
}

/// Iterator over the 1 bits of the board, pops the least significant bit each iteration
impl Iterator for BitBoard {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if *self == Self::EMPTY {
            None
        } else {
            let sq = self.lsb();
            self.0 &= self.0 - 1;

            Some(sq)
        }
    }
}

impl BitBoard {
    pub const EMPTY: Self = Self(0);
    pub const FULL: Self = Self(0xFFFFFFFFFFFFFFFF);
    pub const EMPTY_BB64: BB64 = [Self::EMPTY; Square::COUNT];

    // Rank masks
    pub const START_RANKS: [Self; 2] = [Self(0x00FF000000000000), Self(0x000000000000FF00)];
    pub const EP_RANKS: [Self; 2] = [Self(0x00000000FF000000), Self(0x000000FF00000000)];
    pub const PROMO_RANKS: [Self; 2] = [Self(0x000000000000FF00), Self(0x00FF000000000000)];

    /// Check whether given square is set on the board
    pub const fn get_bit(self, square: Square) -> bool {
        self.0 & (1u64 << square as usize) != 0
    }

    /// Sets given square on the board
    pub const fn set_bit(self, square: Square) -> Self {
        Self(self.0 | 1u64 << square as usize)
    }

    /// Pops given square off the board
    pub const fn pop_bit(self, square: Square) -> Self {
        Self(self.0 & !(1u64 << square as usize))
    }

    /// Flip the bitboard vertically.
    /// Only used because Syzygy TBs index the first bit with A1, not A8
    pub const fn flipv(self) -> Self {
        Self(self.0.swap_bytes())
    }

    /// Shift the bitboard one rank forward for the side to move.
    pub const fn forward(self, side: Color) -> Self {
        match side {
            Color::White => Self(self.0 >> 8),
            Color::Black => Self(self.0 << 8),
        }
    }

    /// Returns popcnt
    /// Using RUSTFLAGS='target-cpu=native' we enforce the popcnt feature
    pub const fn count_bits(self) -> u32 {
        self.0.count_ones()
    }

    /// Returns first set square from board (least significant 1 bit)
    pub const fn lsb(self) -> Square {
        transmute_enum!(self.0.trailing_zeros() as u8, 63)
    }
}
