/// Chess module contains all structures to fully represent the game of chess
pub mod bitboard;
pub mod board;
pub mod castle;
pub mod move_list;
pub mod moves;
pub mod piece;
pub mod square;
pub mod tables;
pub mod zobrist;

pub use tables::init_all_tables;

/// Macro used to transmute enums to their binary representation.
/// This is needed to make most enum functions compile-time constants (c++ constexpr).
///
///     x  --> enum value in correct binary representation
///   mask --> bitmask to get only the relevant bits for the representation
///
/// UB: as long as the enum in use is #[repr(mask)] this cannot fail
#[macro_export]
macro_rules! transmute_enum {
    ($x:expr, $mask:expr) => {
        unsafe { std::mem::transmute($x & $mask) }
    };
}
