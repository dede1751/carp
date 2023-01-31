/// Module for initializing various global constants
///
/// Carp uses plain magic bitboards found through random guessing for slider piece attack tables.
/// Magic generation code is in earlier commits, it's left out since there is no real use in
/// changing them.
/// Move tables are used for attacks only and exclude pawn quiet moves, which are calculated on
/// the fly by the move generator.
/// I spent a lot of time trying to get const evaluation for the tables to work, but it took far
/// too long to compile and made the code extremely messy. Since I did not want to go the code
/// generation route, I ported the static mut idea from Weiawaga.
mod attacks;
mod magics;

#[rustfmt::skip]
mod constants;

use attacks::*;
pub use constants::*;
use magics::*;

use crate::{bitboard::*, piece::Color, square::*};

/// Initialize all attack tables
pub fn init_all_tables() {
    unsafe {
        TABLES.init();
        BISHOP_MAGICS.init();
        ROOK_MAGICS.init();
    }
}

/// Gets pawn attacks from tables
pub fn pawn_attacks(square: Square, side: Color) -> BitBoard {
    unsafe { TABLES.pawn_attacks[side as usize][square as usize] }
}

/// Gets knight attacks from tables
pub fn knight_attacks(square: Square) -> BitBoard {
    unsafe { TABLES.knight_attacks[square as usize] }
}

/// Gets king attacks from tables
pub fn king_attacks(square: Square) -> BitBoard {
    unsafe { TABLES.king_attacks[square as usize] }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bishop_table_attacks() {
        init_all_tables();
        let bb1 = bishop_attacks(Square::E4, BitBoard(1161084283129857));
        let bb2 = bishop_attacks(Square::B7, BitBoard(35253091631104));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1169881047499265));
        assert_eq!(bb2, BitBoard(68854022149));
    }

    #[test]
    fn rook_table_attacks() {
        init_all_tables();
        let bb1 = rook_attacks(Square::A8, BitBoard(1099511627778));
        let bb2 = rook_attacks(Square::E4, BitBoard(76561335399223296));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1103823438082));
        assert_eq!(bb2, BitBoard(4521393946365952));
    }
}
