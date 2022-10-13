use std::cell::RefCell;

use crate::bitboard::*;
use crate::square::*;
use crate::attacks::*;


#[derive(Debug)]
pub struct Magics {
    bishop: [BitBoard; SQUARE_COUNT],
    rook: [BitBoard; SQUARE_COUNT],
}

/// Implement saving magics and precomputed attacks to files
impl Magics {
    pub fn generate_new() -> Magics {
        let mut magics: Magics = Magics {
            bishop: [EMPTY_BOARD; SQUARE_COUNT],
            rook: [EMPTY_BOARD; SQUARE_COUNT],
        };
        let generator = Rng::new();

        for square in ALL_SQUARES {
            magics.bishop[square.index()] = find_magic(Piece::Bishop(square), &generator);
            magics.rook[square.index()] = find_magic(Piece::Rook(square), &generator);
        };

        magics
    }
}

/// Consistent RNG (easier to follow tutorial)
const STATE: u32 = 1804289383;
struct Rng {
    state: RefCell<u32>,
}

impl Rng {
    pub fn new() -> Rng {
        Rng { state: RefCell::new(STATE) }
    }

    fn rand_u32(&self) -> u32 {
        let mut num = self.state.borrow_mut();
    
        *num ^= *num << 13;
        *num ^= *num >> 17;
        *num ^= *num << 5;

        *num
    }

    fn rand_u64(&self) -> BitBoard {
        let (a, b, c, d) = (
            (self.rand_u32() & 0xFFFF) as u64,  // slice first 16 bits
            (self.rand_u32() & 0xFFFF) as u64,
            (self.rand_u32() & 0xFFFF) as u64,
            (self.rand_u32() & 0xFFFF) as u64);
        
        BitBoard(a | b << 16 | c << 32 | d << 48)
    }

    fn rand_low_bit(&self) -> BitBoard {
        self.rand_u64() & self.rand_u64() & self.rand_u64()
    }
}

/*
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
*/

/// Mask relevant bishop occupancy bits
pub fn bishop_occupancy(square: Square) -> BitBoard {
    ALL_SQUARES.iter()
               .filter(|tgt| { 
        let (tgt_file, tgt_rank) = (tgt.file(), tgt.rank());
        let dist: (i8, i8) = square.dist(*tgt);

        tgt_file != A && tgt_file != H && tgt_rank != One && tgt_rank != Eight && // not edges
        dist.0.abs() == dist.1.abs() &&                                           // diagonal
        **tgt != square
    })
               .fold(EMPTY_BOARD, |mask, square| { mask ^ square.to_board() })
}

/// Mask relevant rook occupancy bits
pub fn rook_occupancy(square: Square) -> BitBoard {
    ALL_SQUARES.iter()
               .filter(|tgt| { 
        let (tgt_file, tgt_rank) = (tgt.file(), tgt.rank());
        let dist: (i8, i8) = square.dist(*tgt);

        ((dist.0 == 0 && tgt_rank != One && tgt_rank != Eight ) ||  // same file
        (dist.1 == 0 && tgt_file != A && tgt_file != H )) &&        // same rank
        **tgt != square
    })
               .fold(EMPTY_BOARD, |mask, square| { mask ^ square.to_board() })
}

/// Mask index only onto the set bits of the board.
/// 
///     index = 0                        -->  all occupancy bits will be unset
///     index = 2^mask.count_bits() - 1  -->  all occupancy bits will be set
/// 
///     Anything between has some bits set, some unset, and covers all possible combinations
///     of 0s and 1s on the set squares of mask
pub fn set_occupancy(mask: BitBoard, index: usize) -> BitBoard {
    IntoIterator::into_iter(mask)
        .enumerate()
        .filter(|(count, _)| { index & (1 << count) != 0 })
        .fold(EMPTY_BOARD, |mask, (_, square)| { mask ^ square.to_board() })
}

#[inline]
fn magic_map(mask: BitBoard , magic: BitBoard, bits: u32) -> usize {
    ((mask * magic).0 >> (64 - bits)) as usize
}

enum Piece{
    Bishop(Square),
    Rook(Square),
}
const ATTEMPTS: u64 = 100000000;
const UPPER_16_BITS: BitBoard = BitBoard(0xFF00000000000000);

fn find_magic(piece: Piece, generator: &Rng) -> BitBoard {
    let (mask, square): (BitBoard, Square) = match piece {
        Piece::Bishop(square) => (bishop_occupancy(square), square),
        Piece::Rook(square)   => (rook_occupancy(square), square)
    };

    // generate all possible blockers for square
    let bits: u32 = mask.count_bits();
    let max_index: usize = 1 << bits;
    let occupancies= (0..max_index).map(|x| { set_occupancy(mask, x) });
    
    // get attack mask for each blocker configuration
    let attacks: Vec<(BitBoard, BitBoard)> = match piece {
        Piece::Bishop(_) => occupancies.map(|x| { (x, mask_bishop_attacks(square, x)) })
                                       .collect(),
        Piece::Rook(_)   => occupancies.map(|x| { (x, mask_rook_attacks(square, x)) })
                                       .collect(),
    };
    
    for _ in 0..ATTEMPTS {
        let magic: BitBoard = generator.rand_low_bit();
        
        // we need the mask magic mapping to have at least 6 bits in the upper 16
        if ((mask * magic) & UPPER_16_BITS).count_bits() >= 6 {
            let mut used: [BitBoard; 4096] = [EMPTY_BOARD; 4096]; // 2^12 max occupancies
            let mut found: bool = true;
            
            // try using magic to map all attack masks
            for (occupancy, attack) in &attacks {
                let magic_index = magic_map(*occupancy, magic, bits);
                
                if used[magic_index] == EMPTY_BOARD {
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
    EMPTY_BOARD
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn bishop_mask(){
        let bb1: BitBoard = bishop_occupancy(A2);
        let bb2: BitBoard = bishop_occupancy(D8);
        let bb3: BitBoard = bishop_occupancy(H1);
        let bb4: BitBoard = bishop_occupancy(E4);
        
        assert_eq!(bb1, BitBoard(2216338399232));
        assert_eq!(bb2, BitBoard(1075975168));
        assert_eq!(bb3, BitBoard(18049651735527936));
        assert_eq!(bb4, BitBoard(19184279556981248));
    }

    #[test]
    fn rook_mask(){
        let bb1: BitBoard = rook_occupancy(A8);
        let bb2: BitBoard = rook_occupancy(B7);
        let bb3: BitBoard = rook_occupancy(H2);
        let bb4: BitBoard = rook_occupancy(E4);

        println!("{}", bb1);
        println!("{}", bb2);
        println!("{}", bb3);
        println!("{}", bb4);

        assert_eq!(bb1, BitBoard(282578800148862));
        assert_eq!(bb2, BitBoard(565157600328704));
        assert_eq!(bb3, BitBoard(35607136465616896));
        assert_eq!(bb4, BitBoard(4521664529305600));
    }
}