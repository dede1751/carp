#![allow(dead_code)]
//! # Carp chess engine by Andrea Sgobbi
//! 
//! This is a didactic chess engine for both approaching chess programming and learning to code
//! in Rust.
//! 
//! 3606 loc

pub mod bitboard;
pub mod square;
pub mod piece;
pub mod moves;
pub mod move_order;

pub mod castling_rights;
pub mod board_repr;
pub mod zobrist;

pub mod tables;
pub mod tt;
pub mod search;
pub mod evaluation;
pub mod clock;
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
        unsafe { transmute($x & $mask) }
    };
}

fn main() {
    UCIController::new().run();
}