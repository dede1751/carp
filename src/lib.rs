#![allow(dead_code)]
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
mod board_repr;

mod tables;
mod uci;
mod evaluation;
mod search;
mod move_order;

pub use bitboard::*;
pub use square::*;
pub use piece::*;
pub use moves::*;
pub use castling_rights::*;
pub use board_repr::*;

pub use tables::*;

pub use uci::*;
pub use search::*;
pub use evaluation::*;
pub use move_order::*;