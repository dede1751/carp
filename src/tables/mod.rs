/// Module for initializing various global constants
/// 
/// The module exposes the precalculated attack tables, but also includes functionality to generate
/// new zobrist keys and evaluation tables, to replace the hardcoded ones.
/// 
/// Carp uses plain magic bitboards found through random guessing for slider piece attack tables.
/// RNG for the guessing is built-in and consistent (no need for security), magic values are
/// hardcoded into the engine because there really is no need to change them. The code to generate
/// them remains in the rng module.
/// Move tables are used for attacks only: quiet moves are calculated on the fly by the move
/// generator.

mod attacks;
mod magics;
mod customize;

use attacks::*;
use magics::*;

use crate::bitboard::*;
use crate::square::*;
use crate::piece::Color;

const BISHOP_OCCUPANCY_BITS: [u32; SQUARE_COUNT] = [
    6, 5, 5, 5, 5, 5, 5, 6,
    5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 5, 5, 5, 5, 5, 5, 6,
];
const ROOK_OCCUPANCY_BITS: [u32; SQUARE_COUNT] = [
    12, 11, 11, 11, 11, 11, 11, 12,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    12, 11, 11, 11, 11, 11, 11, 12,
];

/// Precalculated attack tables for all pieces
#[derive(Debug)]
pub struct Tables {
    pawn_attacks  : [BB64; 2],
    knight_attacks: BB64,
    king_attacks  : BB64,

    bishop_magics: BB64,
    bishop_occupancies: BB64,
    bishop_attacks: Vec<Vec<BitBoard>>,

    rook_magics: BB64,
    rook_occupancies: BB64,
    rook_attacks: Vec<Vec<BitBoard>>,
}

impl Default for Tables {
    fn default() -> Self {
        let mut tables = Tables{
            pawn_attacks  : [EMPTY_BB64; 2],
            knight_attacks: EMPTY_BB64,
            king_attacks  : EMPTY_BB64,

            bishop_magics: DEFAULT_BISHOP_MAGICS,
            bishop_occupancies: EMPTY_BB64,
            bishop_attacks    : Vec::new(),

            rook_magics: DEFAULT_ROOK_MAGICS,
            rook_occupancies: EMPTY_BB64,
            rook_attacks    : Vec::new(),
        };

        tables.init_leaper_attacks();
        tables.init_slider_attacks();

        tables
    }
}

/// Table initialization
impl Tables {
    // initializes leaper tables
    fn init_leaper_attacks(&mut self) {
        for square in ALL_SQUARES {
            let rank = square.rank();
            let sq = square as usize;
    
            // pawn attacks are also generated behind the starting rank, needed to be able to check
            // quickly for attacks when the opposite colors pawns are one rank from promotion
            if rank != Rank::Eight {
                self.pawn_attacks[0][sq] = mask_pawn_attacks(square, Color::White);
            }
            if rank != Rank::First {
                self.pawn_attacks[1][sq] = mask_pawn_attacks(square, Color::Black);
            }
            self.knight_attacks[sq] = mask_knight_attacks(square);
            self.king_attacks[sq] = mask_king_attacks(square);
        }
    }

    // initializes slider tables using magics
    fn init_slider_attacks(&mut self) {
        for square in ALL_SQUARES {
            // gen bishop tables and occupancies
            let mask = bishop_occupancy(square);
            let relevant_bits = mask.count_bits();
            let mut occ_map: Vec<BitBoard> = vec!(EMPTY_BB; 1 << relevant_bits);

            for index in 0..(1 << relevant_bits) {
                let blockers = set_occupancy(mask, index);
                let magic_index = magic_map(
                    blockers, self.bishop_magics[square as usize], relevant_bits);
                
                occ_map[magic_index] = mask_bishop_attacks(square, blockers);
            };

            self.bishop_occupancies[square as usize] = mask;
            self.bishop_attacks.push(occ_map);

            // gen rook tables and occupancies
            let mask = rook_occupancy(square);
            let relevant_bits = mask.count_bits();
            let mut occ_map: Vec<BitBoard> = vec!(EMPTY_BB; 1 << relevant_bits);

            for index in 0..(1 << relevant_bits) {
                let blockers = set_occupancy(mask, index);
                let magic_index = magic_map(
                    blockers, self.rook_magics[square as usize], relevant_bits);
                
                occ_map[magic_index] = mask_rook_attacks(square, blockers);
            };

            self.rook_occupancies[square as usize] = mask;
            self.rook_attacks.push(occ_map);
        }
    } 
}

/// Table lookup
impl Tables {
    /// Gets pawn attacks from tables
    #[inline]
    pub fn get_pawn_attack(&self, square: Square, side: Color) -> BitBoard {
        match side {
            Color::White => self.pawn_attacks[0][square as usize],
            Color::Black => self.pawn_attacks[1][square as usize],
        }
    }

    /// Gets knight attacks from tables
    #[inline]
    pub fn get_knight_attack(&self, square: Square) -> BitBoard {
        self.knight_attacks[square as usize]
    }

    /// Gets king attacks from tables
    #[inline]
    pub fn get_king_attack(&self, square: Square) -> BitBoard {
        self.king_attacks[square as usize]
    }

    /// Gets bishop attacks based on the blocker bitboard
    #[inline]
    pub fn get_bishop_attack(&self, square: Square, blockers: BitBoard) -> BitBoard {
        let sq = square as usize;
        let bits = BISHOP_OCCUPANCY_BITS[sq];
        let magic_index = magic_map(
            blockers & self.bishop_occupancies[sq],
            self.bishop_magics[sq],
            bits);

        self.bishop_attacks[sq][magic_index]
    }

    /// Gets rook attacks based on the blocker bitboard
    #[inline]
    pub fn get_rook_attack(&self, square: Square, blockers: BitBoard) -> BitBoard {
        let sq = square as usize;
        let bits = ROOK_OCCUPANCY_BITS[sq];
        let magic_index = magic_map(
            blockers & self.rook_occupancies[sq],
            self.rook_magics[sq],
            bits);

        self.rook_attacks[sq][magic_index]
    }

    /// Gets queen attacks based on the blocker bitboard
    #[inline]
    pub fn get_queen_attack(&self, square: Square, blockers: BitBoard) -> BitBoard {
        self.get_rook_attack(square, blockers) | self.get_bishop_attack(square, blockers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bishop_attacks(){
        let t = Tables::default();

        let bb1: BitBoard = t.get_bishop_attack(Square::E4, BitBoard(1161084283129857));
        let bb2: BitBoard = t.get_bishop_attack(Square::B7, BitBoard(35253091631104));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1169881047499265));
        assert_eq!(bb2, BitBoard(68854022149));
    }

    #[test]
    fn rook_attacks(){
        let t = Tables::default();

        let bb1: BitBoard = t.get_rook_attack(Square::A8, BitBoard(1099511627778));
        let bb2: BitBoard = t.get_rook_attack(Square::E4, BitBoard(76561335399223296));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1103823438082));
        assert_eq!(bb2, BitBoard(4521393946365952));
    }
}