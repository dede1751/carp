/// Chess crate contains all structures to fully represent the game of chess
/// This is not meant to be a generalized library, but rather a backend specifically for Carp.
pub mod bitboard;
pub mod board;
pub mod castle;
pub mod move_list;
pub mod moves;
pub mod piece;
pub mod square;
pub mod zobrist;

/// Macro used to transmute enums to their binary representation.
/// This is needed to make most enum functions compile-time constants (c++ constexpr).
///
///     x  --> enum value in correct binary representation
///   mask --> bitmask to get only the relevant bits for the representation
///
/// UB: as long as the enum in use is #[repr(mask)] this cannot fail
macro_rules! transmute_enum {
    ($x:expr, $mask:expr) => {
        unsafe { std::mem::transmute($x & $mask) }
    };
}

pub(crate) use transmute_enum;
