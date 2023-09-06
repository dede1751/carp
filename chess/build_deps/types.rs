/// Dummy types used by the build script.

#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct BitBoard(pub u64);

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
    BitXor::bitxor,
    Shl::shl,
    Shr::shr
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
    type Output = BitBoard;

    fn not(self) -> BitBoard {
        BitBoard(!self.0)
    }
}

impl std::fmt::Display for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = format!("\n      Bitboard: {}\n", self.0);

        for square in 0..64 {
            if square % 8 == 0 {
                s.push_str(format!("\n{}   ", (8 - square / 8)).as_str())
            }

            if self.0 & 1 << square as u64 != 0 {
                s.push_str("X ");
            } else {
                s.push_str("- ");
            }
        }
        s.push_str("\n\n    A B C D E F G H");
        write!(f, "{s}")
    }
}

impl Iterator for BitBoard {
    type Item = Square;

    fn next(&mut self) -> Option<Square> {
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
    pub const A_FILE: Self = Self(0x0101010101010101);
    pub const H_FILE: Self = Self(0x8080808080808080);

    pub fn north(self) -> Self {
        self >> Self(8)
    }

    pub fn east(self) -> Self {
        (self & !Self::H_FILE) << Self(1)
    }

    pub fn south(self) -> Self {
        self << Self(8)
    }

    pub fn west(self) -> Self {
        (self & !Self::A_FILE) >> Self(1)
    }

    pub fn north_east(self) -> Self {
        (self & !Self::H_FILE) >> Self(7)
    }

    pub fn south_east(self) -> Self {
        (self & !Self::H_FILE) << Self(9)
    }

    pub fn south_west(self) -> Self {
        (self & !Self::A_FILE) << Self(7)
    }

    pub fn north_west(self) -> Self {
        (self & !Self::A_FILE) >> Self(9)
    }

    pub const fn lsb(self) -> Square {
        unsafe { std::mem::transmute(self.0.trailing_zeros() as u8) }
    }

    pub fn set_occupancy(self, index: usize) -> BitBoard {
        self.into_iter()
            .enumerate()
            .filter(|(count, _)| index & (1 << count) != 0)
            .fold(BitBoard::EMPTY, |bb, (_, square)| bb | square.to_board())
    }
}

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

impl Square {
    pub const COUNT: usize = 64;

    #[rustfmt::skip]
    pub const ALL: [Self; Self::COUNT] = [
        A8, B8, C8, D8, E8, F8, G8, H8,
        A7, B7, C7, D7, E7, F7, G7, H7,
        A6, B6, C6, D6, E6, F6, G6, H6,
        A5, B5, C5, D5, E5, F5, G5, H5,
        A4, B4, C4, D4, E4, F4, G4, H4,
        A3, B3, C3, D3, E3, F3, G3, H3,
        A2, B2, C2, D2, E2, F2, G2, H2,
        A1, B1, C1, D1, E1, F1, G1, H1,
    ];

    pub const fn to_board(self) -> BitBoard {
        BitBoard(1 << self as u8)
    }

    pub const fn jump<const FILE: i8, const RANK: i8>(self) -> Option<Self> {
        let (file, rank) = ((self as i8 & 7) + FILE, (self as i8 >> 3) - RANK);

        if file >= 0 && file < 8 && rank >= 0 && rank < 8 {
            Some(unsafe { std::mem::transmute((rank << 3) + file) })
        } else {
            None
        }
    }
}
