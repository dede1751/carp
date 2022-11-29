//! # Implement consistent RNG and generate some constants
//! 
//! Both magics and Zobrist keys are saved as constants in the source code since there is no real
//! need to create new ones. All functions dedicated to their generation are saved in this module.

use std::cell::RefCell;

use crate::{
    bitboard::*,
    square::*,
    piece::*,
};
use super::{
    attacks::*,
    magics::*,
};

const DEFAULT_STATE: u32 = 1804289383;

/// Consistent RNG through xor shift
struct Rng {
    state: RefCell<u32>,
}

impl Default for Rng {
    fn default() -> Self {
        Rng { state: RefCell::new(DEFAULT_STATE) }
    }
}

impl Rng {
    fn rand_u32(&self) -> u32 {
        let mut num = self.state.borrow_mut();
    
        *num ^= *num << 13;
        *num ^= *num >> 17;
        *num ^= *num << 5;

        *num
    }

    pub fn rand_u64(&self) -> u64 {
        let (a, b, c, d) = (
            (self.rand_u32() & 0xFFFF) as u64,  // slice first 16 bits
            (self.rand_u32() & 0xFFFF) as u64,
            (self.rand_u32() & 0xFFFF) as u64,
            (self.rand_u32() & 0xFFFF) as u64);
        
        a | b << 16 | c << 32 | d << 48
    }

    pub fn rand_low_bit(&self) -> u64 {
        self.rand_u64() & self.rand_u64() & self.rand_u64()
    }
}


///////////////////////////////////////////////////////////////////////////////////////////////////
///     ZOBRIST --- Prints out zobrist key arrays
fn _generate_zobrist_keys() {
    let rng = Rng::default();

    let mut piece_keys = [[0; SQUARE_COUNT]; PIECE_COUNT];
    let mut ep_keys = [0; SQUARE_COUNT];
    let mut castle_keys = [0; 16];
    let side_key = rng.rand_u64();

    piece_keys
        .iter_mut()
        .for_each(|bb| {
            bb.iter_mut()
                .for_each(|h|{
                    *h = rng.rand_u64()
                })
        });
    ep_keys.iter_mut().for_each(|h| { *h = rng.rand_u64() });
    castle_keys.iter_mut().for_each(|h| { *h = rng.rand_u64() });

    println!("{:?}\n\n{:?}\n\n{:?}\n\n{:?}", piece_keys, ep_keys, castle_keys, side_key);
}


///////////////////////////////////////////////////////////////////////////////////////////////////
///     MAGICS -- Returns (bishop, rook) magic number arrays
fn _generate_magics() -> (BB64, BB64) {
    let mut magics = (EMPTY_BB64, EMPTY_BB64);
    let generator = Rng::default();

    for square in ALL_SQUARES {
        magics.0[square.index()] = find_magic(Piece::Bishop(square), &generator);
        magics.1[square.index()] = find_magic(Piece::Rook(square), &generator);
    };

    magics
}

/// piece enum used only for readability
enum Piece{
    Bishop(Square),
    Rook(Square),
}
const ATTEMPTS: u64 = 100000000;
const UPPER_BYTE: BitBoard = BitBoard(0xFF00000000000000);

/// Finds magic bitboard for the given piece on the given square.
/// 
/// Brute force guesses until we find a magic that bijectively maps all possible attack maps from
/// the square. First successful magic is returned.
fn find_magic(piece: Piece, generator: &Rng) -> BitBoard {
    let (mask, square): (BitBoard, Square) = match piece {
        Piece::Bishop(square) => (bishop_occupancy(square), square),
        Piece::Rook(square)   => (rook_occupancy(square), square)
    };

    // generate all possible blockers for square
    let bits: u32 = mask.count_bits();
    let max_index: usize = 1 << bits;
    let occupancies= (0..max_index).map(|x| { 
        set_occupancy(mask, x) });
    
    // get attack mask for each blocker configuration (occupancy, attacks)
    let attacks: Vec<(BitBoard, BitBoard)> = match piece {
        Piece::Bishop(_) => occupancies.map(|x| { (x, mask_bishop_attacks(square, x)) })
                                       .collect(),
        Piece::Rook(_)   => occupancies.map(|x| { (x, mask_rook_attacks(square, x)) })
                                       .collect(),
    };
    
    for _ in 0..ATTEMPTS {
        let magic: BitBoard = BitBoard(generator.rand_low_bit());
        
        // we need the mask magic mapping to have at least 6 bits in the upper byte
        if ((mask * magic) & UPPER_BYTE).count_bits() >= 6 {
            let mut used: [BitBoard; 4096] = [EMPTY_BB; 4096]; // 2^12 max occupancies
            let mut found: bool = true;
            
            // try using magic to map all attack masks
            for (occupancy, attack) in &attacks {
                let magic_index = magic_map(*occupancy, magic, bits);
                
                if used[magic_index] == EMPTY_BB {
                    used[magic_index] = *attack;
                } else if used[magic_index] != *attack {
                    found = false;
                    break;
                }
            }

            if found { return magic }
        }
    }
    
    println!("FAILED!");
    EMPTY_BB
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn magics() {
        let magic = find_magic(Piece::Bishop(Square::E4), &Rng::default());

        assert_ne!(magic, EMPTY_BB); // if magic is 0, we didn't find it

        let occs = bishop_occupancy(Square::E4);
        let bits: usize = 9;
 
        let mut indices: Vec<usize> = (0..(1 << bits))
                .map(|index| {
            let blockers = set_occupancy(occs, index);
            println!("{} {}", occs, blockers);
            magic_map(blockers, magic, bits as u32)
        })
                .collect();

        indices.sort();
        assert_eq!(indices, (0..(1 << bits)).collect::<Vec<usize>>()) // mapping is bijective
    }
}
