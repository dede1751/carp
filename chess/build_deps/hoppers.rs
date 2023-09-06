/// Mask attacks for hopping pieces.
use super::types::{BitBoard, Square};

pub trait Hopping {
    /// Return the list of hops the piece can make.
    fn hops() -> Vec<fn(Square) -> Option<Square>>;

    /// Generate (slowly) all attacks for this hopper.
    fn slow_attacks() -> [BitBoard; Square::COUNT] {
        let mut attacks = [BitBoard::EMPTY; Square::COUNT];

        for src in Square::ALL {
            let mut targets = BitBoard::EMPTY;

            for hop in Self::hops() {
                if let Some(tgt) = hop(src) {
                    targets |= tgt.to_board();
                }
            }
            attacks[src as usize] = targets;
        }

        attacks
    }
}

pub struct King;
impl Hopping for King {
    fn hops() -> Vec<fn(Square) -> Option<Square>> {
        vec![
            Square::jump::<0, 1>,
            Square::jump::<1, 1>,
            Square::jump::<1, 0>,
            Square::jump::<1, -1>,
            Square::jump::<0, -1>,
            Square::jump::<-1, -1>,
            Square::jump::<-1, 0>,
            Square::jump::<-1, 1>,
        ]
    }
}

pub struct Knight;
impl Hopping for Knight {
    fn hops() -> Vec<fn(Square) -> Option<Square>> {
        vec![
            Square::jump::<1, 2>,
            Square::jump::<2, 1>,
            Square::jump::<2, -1>,
            Square::jump::<1, -2>,
            Square::jump::<-1, -2>,
            Square::jump::<-2, -1>,
            Square::jump::<-2, 1>,
            Square::jump::<-1, 2>,
        ]
    }
}

pub const WHITE: i8 = 1;
pub const BLACK: i8 = -1;

pub struct Pawn<const COLOR: i8>;
impl<const COLOR: i8> Hopping for Pawn<COLOR> {
    fn hops() -> Vec<fn(Square) -> Option<Square>> {
        vec![Square::jump::<1, COLOR>, Square::jump::<-1, COLOR>]
    }
}
