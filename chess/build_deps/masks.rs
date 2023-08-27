/// Mask attacks and relevant occupancies for sliding pieces.
use super::types::{BitBoard, Square};

/// Trait defining a sliding piece.
/// A sliding piece simply has a set of directions (fn(BB) -> BB) that it can use to move.
/// From the directions attacks and relevant occupancies are inferred.
pub trait Sliding {
    /// Return the list of directions the type can move in.
    fn directions() -> Vec<fn(BitBoard) -> BitBoard>;

    /// Return the relevant occupancies for the type.
    /// Relevant occupancies are the squares for which the presence of a piece influences the attacls.
    fn relevant_occupancies(sq: Square) -> BitBoard {
        let mut occs = BitBoard::EMPTY;

        for step in Self::directions() {
            let mut tgt = step(sq.to_board());

            while step(tgt) != BitBoard::EMPTY {
                occs |= tgt;
                tgt = step(tgt);
            }
        }

        occs
    }

    /// Generate (slowly) an attack bitboard for this slider given the set of blockers.
    fn slow_attacks(sq: Square, blockers: BitBoard) -> BitBoard {
        let mut attacks = BitBoard::EMPTY;

        for step in Self::directions() {
            let mut tgt = sq.to_board();

            while tgt & !blockers != BitBoard::EMPTY {
                tgt = step(tgt);
                attacks |= tgt;
            }
        }

        attacks
    }
}

/// Bishops are sliders that can move diagonally.
pub struct Bishop;

impl Sliding for Bishop {
    fn directions() -> Vec<fn(BitBoard) -> BitBoard> {
        vec![
            BitBoard::north_east,
            BitBoard::south_east,
            BitBoard::south_west,
            BitBoard::north_west,
        ]
    }
}

/// Rooks are sliders that can move orthogonally.
pub struct Rook;

impl Sliding for Rook {
    fn directions() -> Vec<fn(BitBoard) -> BitBoard> {
        vec![
            BitBoard::north,
            BitBoard::east,
            BitBoard::south,
            BitBoard::west,
        ]
    }
}
