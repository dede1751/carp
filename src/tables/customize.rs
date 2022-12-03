//! # Customize various constants hardcoded into the program
//! 
//! Both magics and Zobrist keys are saved as constants in the source code since there is no real
//! need to create new ones. All functions dedicated to their generation are saved in this module.
//! 
//! Evaluation can be modified by changing various PSTs

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
        magics.0[square as usize] = find_magic(Piece::Bishop(square), &generator);
        magics.1[square as usize] = find_magic(Piece::Rook(square), &generator);
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


///////////////////////////////////////////////////////////////////////////////////////////////////
/// PESTO EVAL --- Customize the tables here:
type Eval = i32;
const MG_PIECE_VALUES: [Eval; 6] = [ 82, 337, 365, 477, 1025, 12000 ];
const EG_PIECE_VALUES: [Eval; 6] = [ 94, 281, 297, 512,  936, 12000 ];

const MG_PAWN: [Eval; SQUARE_COUNT] = [
    0,   0,   0,   0,   0,   0,  0,   0,
   98, 134,  61,  95,  68, 126, 34, -11,
   -6,   7,  26,  31,  65,  56, 25, -20,
  -14,  13,   6,  21,  23,  12, 17, -23,
  -27,  -2,  -5,  12,  17,   6, 10, -25,
  -26,  -4,  -4, -10,   3,   3, 33, -12,
  -35,  -1, -20, -23, -15,  24, 38, -22,
    0,   0,   0,   0,   0,   0,  0,   0
];

const EG_PAWN: [Eval; SQUARE_COUNT] = [
    0,   0,   0,   0,   0,   0,   0,   0,
  178, 173, 158, 134, 147, 132, 165, 187,
   94, 100,  85,  67,  56,  53,  82,  84,
   32,  24,  13,   5,  -2,   4,  17,  17,
   13,   9,  -3,  -7,  -7,  -8,   3,  -1,
    4,   7,  -6,   1,   0,  -5,  -1,  -8,
   13,   8,   8,  10,  13,   0,   2,  -7,
    0,   0,   0,   0,   0,   0,   0,   0
];

const MG_KNIGHT: [Eval; SQUARE_COUNT] = [
  -167, -89, -34, -49,  61, -97, -15, -107,
   -73, -41,  72,  36,  23,  62,   7,  -17,
   -47,  60,  37,  65,  84, 129,  73,   44,
    -9,  17,  19,  53,  37,  69,  18,   22,
   -13,   4,  16,  13,  28,  19,  21,   -8,
   -23,  -9,  12,  10,  19,  17,  25,  -16,
   -29, -53, -12,  -3,  -1,  18, -14,  -19,
  -105, -21, -58, -33, -17, -28, -19,  -23
];

const EG_KNIGHT: [Eval; SQUARE_COUNT] = [
  -58, -38, -13, -28, -31, -27, -63, -99,
  -25,  -8, -25,  -2,  -9, -25, -24, -52,
  -24, -20,  10,   9,  -1,  -9, -19, -41,
  -17,   3,  22,  22,  22,  11,   8, -18,
  -18,  -6,  16,  25,  16,  17,   4, -18,
  -23,  -3,  -1,  15,  10,  -3, -20, -22,
  -42, -20, -10,  -5,  -2, -20, -23, -44,
  -29, -51, -23, -15, -22, -18, -50, -64
];

const MG_BISHOP: [Eval; SQUARE_COUNT] = [
  -29,   4, -82, -37, -25, -42,   7,  -8,
  -26,  16, -18, -13,  30,  59,  18, -47,
  -16,  37,  43,  40,  35,  50,  37,  -2,
   -4,   5,  19,  50,  37,  37,   7,  -2,
   -6,  13,  13,  26,  34,  12,  10,   4,
    0,  15,  15,  15,  14,  27,  18,  10,
    4,  15,  16,   0,   7,  21,  33,   1,
  -33,  -3, -14, -21, -13, -12, -39, -21
];

const EG_BISHOP: [Eval; SQUARE_COUNT] = [
  -14, -21, -11,  -8, -7,  -9, -17, -24,
   -8,  -4,   7, -12, -3, -13,  -4, -14,
    2,  -8,   0,  -1, -2,   6,   0,   4,
   -3,   9,  12,   9, 14,  10,   3,   2,
   -6,   3,  13,  19,  7,  10,  -3,  -9,
  -12,  -3,   8,  10, 13,   3,  -7, -15,
  -14, -18,  -7,  -1,  4,  -9, -15, -27,
  -23,  -9, -23,  -5, -9, -16,  -5, -17,
];

const MG_ROOK: [Eval; SQUARE_COUNT] = [
   32,  42,  32,  51, 63,  9,  31,  43,
   27,  32,  58,  62, 80, 67,  26,  44,
   -5,  19,  26,  36, 17, 45,  61,  16,
  -24, -11,   7,  26, 24, 35,  -8, -20,
  -36, -26, -12,  -1,  9, -7,   6, -23,
  -45, -25, -16, -17,  3,  0,  -5, -33,
  -44, -16, -20,  -9, -1, 11,  -6, -71,
  -19, -13,   1,  17, 16,  7, -37, -26
];

const EG_ROOK: [Eval; SQUARE_COUNT] = [
  13, 10, 18, 15, 12,  12,   8,   5,
  11, 13, 13, 11, -3,   3,   8,   3,
   7,  7,  7,  5,  4,  -3,  -5,  -3,
   4,  3, 13,  1,  2,   1,  -1,   2,
   3,  5,  8,  4, -5,  -6,  -8, -11,
  -4,  0, -5, -1, -7, -12,  -8, -16,
  -6, -6,  0,  2, -9,  -9, -11,  -3,
  -9,  2,  3, -1, -5, -13,   4, -20
  ];

const MG_QUEEN: [Eval; SQUARE_COUNT] = [
  -28,   0,  29,  12,  59,  44,  43,  45,
  -24, -39,  -5,   1, -16,  57,  28,  54,
  -13, -17,   7,   8,  29,  56,  47,  57,
  -27, -27, -16, -16,  -1,  17,  -2,   1,
   -9, -26,  -9, -10,  -2,  -4,   3,  -3,
  -14,   2, -11,  -2,  -5,   2,  14,   5,
  -35,  -8,  11,   2,   8,  15,  -3,   1,
   -1, -18,  -9,  10, -15, -25, -31, -50
];

const EG_QUEEN: [Eval; SQUARE_COUNT] = [
   -9,  22,  22,  27,  27,  19,  10,  20,
  -17,  20,  32,  41,  58,  25,  30,   0,
  -20,   6,   9,  49,  47,  35,  19,   9,
    3,  22,  24,  45,  57,  40,  57,  36,
  -18,  28,  19,  47,  31,  34,  39,  23,
  -16, -27,  15,   6,   9,  17,  10,   5,
  -22, -23, -30, -16, -16, -23, -36, -32,
  -33, -28, -22, -43,  -5, -32, -20, -41
];

const MG_KING: [Eval; SQUARE_COUNT] = [
  -65,  23,  16, -15, -56, -34,   2,  13,
   29,  -1, -20,  -7,  -8,  -4, -38, -29,
   -9,  24,   2, -16, -20,   6,  22, -22,
  -17, -20, -12, -27, -30, -25, -14, -36,
  -49,  -1, -27, -39, -46, -44, -33, -51,
  -14, -14, -22, -46, -44, -30, -15, -27,
    1,   7,  -8, -64, -43, -16,   9,   8,
  -15,  36,  12, -54,   8, -28,  24,  14
];

const EG_KING: [Eval; SQUARE_COUNT] = [
  -74, -35, -18, -18, -11,  15,   4, -17,
  -12,  17,  14,  17,  17,  38,  23,  11,
   10,  17,  23,  15,  20,  45,  44,  13,
   -8,  22,  24,  27,  26,  33,  26,   3,
  -18,  -4,  21,  24,  27,  23,   9, -11,
  -19,  -3,  11,  21,  23,  16,   7,  -9,
  -27, -11,   4,  13,  14,   4,  -5, -17,
  -53, -34, -21, -11, -28, -14, -24, -43
];

const MG_PESTO: [[Eval; SQUARE_COUNT]; 6] = [
    MG_PAWN, MG_KNIGHT, MG_BISHOP, MG_ROOK, MG_QUEEN, MG_KING
];
const EG_PESTO: [[Eval; SQUARE_COUNT]; 6] = [
    EG_PAWN, EG_KNIGHT, EG_BISHOP, EG_ROOK, EG_QUEEN, EG_KING
];

pub fn init_tables() {
    let mut mg_table: [[Eval; SQUARE_COUNT]; PIECE_COUNT] = [[0; SQUARE_COUNT]; PIECE_COUNT];
    let mut eg_table: [[Eval; SQUARE_COUNT]; PIECE_COUNT] = [[0; SQUARE_COUNT]; PIECE_COUNT];

    for piece in ALL_PIECES {
        let i = piece as usize >> 1;
        for square in ALL_SQUARES {
            let sq = square as usize;
            let flipped_sq = square.flipv() as usize;

            mg_table[piece as usize][sq] = match piece.color() {
                Color::White => {
                    MG_PIECE_VALUES[i] + MG_PESTO[i][sq]
                },
                Color::Black => {
                    MG_PIECE_VALUES[i] + MG_PESTO[i][flipped_sq]
                }
            };
            eg_table[piece as usize][square as usize] = match piece.color() {
                Color::White => {
                    EG_PIECE_VALUES[i] + EG_PESTO[i][sq]
                },
                Color::Black => {
                    EG_PIECE_VALUES[i] + EG_PESTO[i][flipped_sq]
                }
            };
        }
    }

    println!("MG:\n\n{:?}\n\n\nEG:\n\n{:?}", eg_table, mg_table);
}