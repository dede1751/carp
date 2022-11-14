//! # Carp chess engine
//! 
//! This is a didactic chess engine for both approaching chess programming and learning to code
//! in Rust.
//! 
//! ## EXAMPLE
//! 
//! '''
//! use carp::{ Board, START_FEN };
//! 
//! println!("{}", Board::from_fen(START_FEN));
//! '''

mod bitboard;
mod square;
mod piece;
mod moves;
mod castling_rights;

pub mod tables;
pub mod board_repr;

pub use square::*;
pub use piece::*;
pub use moves::*;
pub use tables::*;
pub use board_repr::*;