/// Module allowing access to various lookup tables.
///
/// For attacks, Carp uses Black Magic BitBoards found by Volker Annuss and Niklas Fiekas
/// https://www.talkchess.com/forum3/viewtopic.php?f=7&t=64790&sid=0cd7ee9568af2cbd4c7297b348b5a850
pub mod magics;

#[rustfmt::skip]
mod constants;

pub use constants::*;
use magics::Magics;

use crate::{bitboard::BitBoard, piece::Color, square::Square};

/// Gets pawn attacks from tables
/// SAFETY: Square and Color only allow valid indices
pub fn pawn_attacks(square: Square, side: Color) -> BitBoard {
    unsafe {
        *PAWN_ATTACKS
            .get_unchecked(side as usize)
            .get_unchecked(square as usize)
    }
}

/// Gets knight attacks from tables
/// SAFETY: Square only allows valid indices
pub fn knight_attacks(square: Square) -> BitBoard {
    unsafe { *KNIGHT_ATTACKS.get_unchecked(square as usize) }
}

/// Gets king attacks from tables
/// SAFETY: Square only allows valid indices
pub fn king_attacks(square: Square) -> BitBoard {
    unsafe { *KING_ATTACKS.get_unchecked(square as usize) }
}

/// Gets bishop attacks based on the blocker bitboard
pub fn bishop_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    Magics::BISHOP.attacks(square, blockers)
}

/// Gets rook attacks based on the blocker bitboard
pub fn rook_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    Magics::ROOK.attacks(square, blockers)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bishop_magic_attacks() {
        let bb1 = bishop_attacks(Square::E4, BitBoard(1161084283129857));
        let bb2 = bishop_attacks(Square::B7, BitBoard(35253091631104));

        println!("{bb1}\n{bb2}\n");

        assert_eq!(bb1, BitBoard(1169881047499265));
        assert_eq!(bb2, BitBoard(68854022149));
    }

    #[test]
    fn rook_magic_attacks() {
        let bb1 = rook_attacks(Square::A8, BitBoard(1099511627778));
        let bb2 = rook_attacks(Square::E4, BitBoard(76561335399223296));

        println!("{bb1}\n{bb2}\n");

        assert_eq!(bb1, BitBoard(1103823438082));
        assert_eq!(bb2, BitBoard(4521393946365952));
    }
}
