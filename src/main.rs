#![allow(dead_code)]

mod bitboard;
use crate::bitboard::*;

mod square;
use crate::square::*;

mod tables;
use tables::*;

mod color;


fn main() {
    let bb: BitBoard = rook_occupancy(D8);

    println!("{}", bb);
}
