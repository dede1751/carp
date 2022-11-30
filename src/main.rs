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
pub mod search;
pub mod evaluation;
pub mod uci;

use crate::uci::UCIController;

fn main() {
    UCIController::new().run();
}