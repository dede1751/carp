/// Module for initializing various global constants
///
/// Carp uses Black Magic BitBoards found by Volker Annuss and Niklas Fiekas
/// https://www.talkchess.com/forum3/viewtopic.php?f=7&t=64790&sid=0cd7ee9568af2cbd4c7297b348b5a850
///
/// Move tables are used for attacks only and exclude pawn quiet moves, which are calculated on
/// the fly by the move generator.
/// I spent a lot of time trying to get const evaluation for the tables to work, but it took far
/// too long to compile and made the code extremely messy. Since I did not want to go the code
/// generation route, I ported the static mut idea from Weiawaga.
mod attacks;
mod magics;

#[rustfmt::skip]
mod constants;

use std::cmp::min;

use attacks::*;
pub use constants::*;
use magics::*;

use crate::chess::{bitboard::*, piece::*, square::*};
use crate::engine::search_params::{LMR_BASE, LMR_FACTOR};

/// Precalculated attack tables for leaper pieces
struct Tables {
    pub pawn_attacks: [BB64; 2],
    pub knight_attacks: BB64,
    pub king_attacks: BB64,
}

static mut TABLES: Tables = Tables {
    pawn_attacks: [EMPTY_BB64; 2],
    knight_attacks: EMPTY_BB64,
    king_attacks: EMPTY_BB64,
};

/// Leaper attack table initialization
impl Tables {
    fn init(&mut self) {
        for square in ALL_SQUARES {
            if square.rank() != Rank::Eight {
                self.pawn_attacks[0][square as usize] = mask_pawn_attacks(square, Color::White);
            }
            if square.rank() != Rank::First {
                self.pawn_attacks[1][square as usize] = mask_pawn_attacks(square, Color::Black);
            }

            self.knight_attacks[square as usize] = mask_knight_attacks(square);
            self.king_attacks[square as usize] = mask_king_attacks(square);
        }
    }
}

/// Precalculated lmr reduction table (values from Asymptote)
/// Using ln(depth) * ln(move_count) we can have near-linear tree growth.
struct LMRTable {
    reductions: [[usize; 64]; 64],
}

static mut LMR_TABLE: LMRTable = LMRTable {
    reductions: [[0; 64]; 64],
};

impl LMRTable {
    fn init(&mut self) {
        for depth in 1..64 {
            for move_count in 1..64 {
                let reduction =
                    LMR_BASE + (depth as f32).ln() * (move_count as f32).ln() / LMR_FACTOR;

                self.reductions[depth][move_count] = reduction as usize;
            }
        }
    }
}

/// Initialize all attack tables
pub fn init_all_tables() {
    unsafe {
        TABLES.init();
        LMR_TABLE.init();
        BISHOP_MAGICS.init();
        ROOK_MAGICS.init();
    }
}

/// Gets pawn attacks from tables
pub fn pawn_attacks(square: Square, side: Color) -> BitBoard {
    unsafe {
        *TABLES
            .pawn_attacks
            .get_unchecked(side as usize)
            .get_unchecked(square as usize)
    }
}

/// Gets knight attacks from tables
pub fn knight_attacks(square: Square) -> BitBoard {
    unsafe { *TABLES.knight_attacks.get_unchecked(square as usize) }
}

/// Gets king attacks from tables
pub fn king_attacks(square: Square) -> BitBoard {
    unsafe { *TABLES.king_attacks.get_unchecked(square as usize) }
}

/// Gets bishop attacks based on the blocker bitboard
pub fn bishop_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    unsafe { BISHOP_MAGICS.attacks(square, blockers) }
}

/// Gets rook attacks based on the blocker bitboard
pub fn rook_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    unsafe { ROOK_MAGICS.attacks(square, blockers) }
}

/// Gets queen attacks based on the blocker bitboard
pub fn queen_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    rook_attacks(square, blockers) | bishop_attacks(square, blockers)
}

/// Gets the lmr reduction given depth and move count
pub fn lmr_reduction(depth: usize, move_count: usize) -> usize {
    let d = min(depth, 63);
    let m = min(move_count.max(1), 63);
    unsafe { *LMR_TABLE.reductions.get_unchecked(d).get_unchecked(m) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bishop_table_attacks() {
        init_all_tables();
        let bb1 = bishop_attacks(Square::E4, BitBoard(1161084283129857));
        let bb2 = bishop_attacks(Square::B7, BitBoard(35253091631104));

        println!("{bb1}\n{bb2}\n");

        assert_eq!(bb1, BitBoard(1169881047499265));
        assert_eq!(bb2, BitBoard(68854022149));
    }

    #[test]
    fn rook_table_attacks() {
        init_all_tables();
        let bb1 = rook_attacks(Square::A8, BitBoard(1099511627778));
        let bb2 = rook_attacks(Square::E4, BitBoard(76561335399223296));

        println!("{bb1}\n{bb2}\n");

        assert_eq!(bb1, BitBoard(1103823438082));
        assert_eq!(bb2, BitBoard(4521393946365952));
    }
}
