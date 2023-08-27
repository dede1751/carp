/// Chess crate contains all structures to fully represent the game of chess
pub mod bitboard;
pub mod board;
pub mod castle;
pub mod move_list;
pub mod moves;
pub mod nnue;
pub mod piece;
pub mod square;
mod tables;
pub mod zobrist;

/// Contains certain engine parameters necessarily kept in the backend.
pub mod params {
    /// Maximum depth supported by the NNUE implementation within the crate.
    pub const MAX_DEPTH: usize = 127;

    /// Eval type returned by the network.
    pub type Eval = i32;

    /// Piece static values used in SEE.
    pub const PIECE_VALUES: [Eval; 12] = [161, 161, 446, 446, 464, 464, 705, 705, 1322, 1322, 0, 0];
}

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
