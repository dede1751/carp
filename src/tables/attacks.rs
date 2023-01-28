/// Slow attack generation to initialize tables
use super::constants::BETWEEN;

use crate::bitboard::*;
use crate::piece::Color;
use crate::square::*;

/// Precalculated attack tables for leaper pieces
pub struct Tables {
    pub pawn_attacks: [BB64; 2],
    pub knight_attacks: BB64,
    pub king_attacks: BB64,
}

pub static mut TABLES: Tables = Tables {
    pawn_attacks: [EMPTY_BB64; 2],
    knight_attacks: EMPTY_BB64,
    king_attacks: EMPTY_BB64,
};

/// Leaper attack table initialization
impl Tables {
    pub fn init(&mut self) {
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

/// Generate bitboard for pawn attacks from square
fn mask_pawn_attacks(src: Square, color: Color) -> BitBoard {
    let mut attacks = EMPTY_BB;
    let file = src.file();

    let tgt = match color {
        Color::White => src.up(),
        Color::Black => src.down(),
    };

    if file != File::A {
        attacks = attacks.set_bit(tgt.left())
    }
    if file != File::H {
        attacks = attacks.set_bit(tgt.right());
    }

    attacks
}

/// Sets all bits that sastisfy the predicate in the bitboard.
fn mask_predicate<P: Fn(&Square) -> bool>(predicate: P) -> BitBoard {
    ALL_SQUARES
        .into_iter()
        .filter(predicate)
        .fold(EMPTY_BB, |att, sq| att.set_bit(sq))
}

/// Generate bitboard for knight attacks from square
pub fn mask_knight_attacks(src: Square) -> BitBoard {
    mask_predicate(|&tgt| {
        let dist = src.dist(tgt);
        let (dist_file, dist_rank) = (dist.0.abs(), dist.1.abs());

        (dist_file == 1 && dist_rank == 2) || (dist_file == 2 && dist_rank == 1)
    })
}

/// Generate bitboard for king attacks from square
pub fn mask_king_attacks(src: Square) -> BitBoard {
    mask_predicate(|&tgt| {
        let dist = src.dist(tgt);

        dist.0.abs() <= 1 && dist.1.abs() <= 1 && tgt != src
    })
}

/// Slow bishop attack gen (with blockers)
pub fn mask_bishop_attacks(src: Square, blockers: BitBoard) -> BitBoard {
    mask_predicate(|&tgt| {
        let dist = src.dist(tgt);

        BETWEEN[src as usize][tgt as usize] & blockers == EMPTY_BB && // not blocked
        dist.0.abs() == dist.1.abs() && src != tgt // bishop movement
    })
}

/// Slow rook attack gen (with blockers)
pub fn mask_rook_attacks(src: Square, blockers: BitBoard) -> BitBoard {
    mask_predicate(|&tgt| {
        let dist = src.dist(tgt);

        BETWEEN[src as usize][tgt as usize] & blockers == EMPTY_BB && // not blocked
        (dist.0 == 0 || dist.1 == 0) && src != tgt // rook movement
    })
}

/// Mask relevant bishop occupancy bits
pub fn bishop_occupancy(src: Square) -> BitBoard {
    mask_predicate(|&tgt| {
        let (tgt_file, tgt_rank) = tgt.coords();
        let dist = src.dist(tgt);

        tgt_file != File::A     && tgt_file != File::H     && // exclude edges
        tgt_rank != Rank::First && tgt_rank != Rank::Eight &&
        dist.0.abs() == dist.1.abs() && src != tgt // bishop movement
    })
}

/// Mask relevant rook occupancy bits
pub fn rook_occupancy(src: Square) -> BitBoard {
    mask_predicate(|&tgt| {
        let (tgt_file, tgt_rank) = tgt.coords();
        let dist = src.dist(tgt);

        ((dist.1 == 0 && tgt_file != File::A     && tgt_file != File::H)      ||
            (dist.0 == 0 && tgt_rank != Rank::First && tgt_rank != Rank::Eight)) && // exclude edges
        src != tgt
    })
}

/// Mask index only onto the set bits of the board.
///
///     index = 0                        -->  all occupancy bits will be unset
///     index = 2^mask.count_bits() - 1  -->  all occupancy bits will be set
///
///     Anything between has some bits set, some unset, and covers all possible combinations
///     of 0s and 1s on the set squares of mask
pub fn set_occupancy(mask: BitBoard, index: usize) -> BitBoard {
    mask.into_iter()
        .enumerate()
        .filter(|(count, _)| index & (1 << count) != 0)
        .fold(EMPTY_BB, |mask, (_, square)| mask.set_bit(square))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pawn_attacks() {
        let bb1 = mask_pawn_attacks(Square::A2, Color::White);
        let bb2 = mask_pawn_attacks(Square::E4, Color::White);
        let bb3 = mask_pawn_attacks(Square::H7, Color::Black);
        let bb4 = mask_pawn_attacks(Square::E7, Color::White);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(2199023255552));
        assert_eq!(bb2, BitBoard(671088640));
        assert_eq!(bb3, BitBoard(4194304));
        assert_eq!(bb4, BitBoard(40));
    }

    #[test]
    fn knight_attacks() {
        let bb1 = mask_knight_attacks(Square::A1);
        let bb2 = mask_knight_attacks(Square::E4);
        let bb3 = mask_knight_attacks(Square::G6);
        let bb4 = mask_knight_attacks(Square::B7);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(1128098930098176));
        assert_eq!(bb2, BitBoard(11333767002587136));
        assert_eq!(bb3, BitBoard(687463207072));
        assert_eq!(bb4, BitBoard(84410376));
    }

    #[test]
    fn king_attacks() {
        let bb1 = mask_king_attacks(Square::A1);
        let bb2 = mask_king_attacks(Square::E4);
        let bb3 = mask_king_attacks(Square::H6);
        let bb4 = mask_king_attacks(Square::D8);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(144959613005987840));
        assert_eq!(bb2, BitBoard(61745389371392));
        assert_eq!(bb3, BitBoard(3225468928));
        assert_eq!(bb4, BitBoard(7188));
    }

    #[test]
    fn bishop_attacks() {
        let bb1 = mask_bishop_attacks(Square::E4, BitBoard(1161084283129857));
        let bb2 = mask_bishop_attacks(Square::B7, BitBoard(35253091631104));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1169881047499265));
        assert_eq!(bb2, BitBoard(68854022149));
    }

    #[test]
    fn rook_attacks() {
        let bb1 = mask_rook_attacks(Square::A8, BitBoard(1099511627778));
        let bb2 = mask_rook_attacks(Square::E4, BitBoard(76561335399223296));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1103823438082));
        assert_eq!(bb2, BitBoard(4521393946365952));
    }

    #[test]
    fn bishop_occs() {
        let bb1 = bishop_occupancy(Square::A2);
        let bb2 = bishop_occupancy(Square::D8);
        let bb3 = bishop_occupancy(Square::H1);
        let bb4 = bishop_occupancy(Square::E4);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(2216338399232));
        assert_eq!(bb2, BitBoard(1075975168));
        assert_eq!(bb3, BitBoard(18049651735527936));
        assert_eq!(bb4, BitBoard(19184279556981248));
    }

    #[test]
    fn rook_occs() {
        let bb1 = rook_occupancy(Square::A8);
        let bb2 = rook_occupancy(Square::B7);
        let bb3 = rook_occupancy(Square::H2);
        let bb4 = rook_occupancy(Square::E4);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(282578800148862));
        assert_eq!(bb2, BitBoard(565157600328704));
        assert_eq!(bb3, BitBoard(35607136465616896));
        assert_eq!(bb4, BitBoard(4521664529305600));
    }
}
