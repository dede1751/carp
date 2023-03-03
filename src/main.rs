/// Carp chess engine by Andrea Sgobbi
///
/// This is a didactic chess engine for both approaching chess programming and learning to code
/// in Rust.
pub mod bitboard;
pub mod move_list;
pub mod moves;
pub mod piece;
pub mod square;
pub mod tables;

pub mod board;
pub mod castle;
pub mod move_sorter;
pub mod position;
pub mod zobrist;

pub mod clock;
pub mod nnue;
pub mod search;
pub mod search_params;
pub mod tt;

pub mod tools;
pub mod uci;

use crate::uci::UCIController;

/// Macro used to transmute enums to their binary representation.
/// This is needed to make most enum functions compile-time constants (c++ constexpr).
///
///     x  --> enum value in correct binary representation
///   mask --> bitmask to get only the relevant bits for the representation
///    
/// Square    : 63 (first 6 bits)
/// Rank/File :  7 (first 3 bits)
/// Piece     : 15 (first 4 bits)
/// Color     :  1 (first bit)
#[macro_export]
macro_rules! from {
    ($x:expr, $mask:expr) => {
        unsafe { std::mem::transmute($x & $mask) }
    };
}

fn main() {
    tables::init_all_tables();
    tools::parse_cli();
    
    UCIController::default().run();
}
