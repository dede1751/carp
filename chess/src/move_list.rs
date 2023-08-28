use crate::{
    moves::{Move, MoveType},
    piece::Color,
    square::Square,
};

/// Simple movelist optimized to cacheline size
#[derive(Clone, Debug)]
pub struct MoveList {
    pub moves: [Move; Self::SIZE],
    len: usize,
}

impl Default for MoveList {
    fn default() -> Self {
        Self {
            moves: [Move::NULL; Self::SIZE],
            len: 0,
        }
    }
}

impl MoveList {
    /// Taken from Pleco
    /// Assuming we have 8 cache lines, which should be 512B, we use up enough to fill a single
    /// cache line with the array and a pointer of our current architecture.
    /// With 2B moves, this results in (512B - {ARCH_POINTER_SIZE}B) / 2B
    #[cfg(target_pointer_width = "128")]
    pub const SIZE: usize = 248;
    #[cfg(target_pointer_width = "64")]
    pub const SIZE: usize = 252;
    #[cfg(target_pointer_width = "32")]
    pub const SIZE: usize = 254;
    #[cfg(any(target_pointer_width = "16", target_pointer_width = "8",))]
    pub const SIZE: usize = 255;

    /// Returns move list length
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns true if the movelist is empty
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Push move to the back of the movelist
    pub fn push(&mut self, m: Move) {
        self.moves[self.len] = m;
        self.len += 1;
    }

    /// Push a pawn quiet move to the back of the movelist (do not use for double push)
    pub fn push_pawn_quiet(&mut self, src: Square, tgt: Square, side: Color) {
        const PROMOTIONS: [MoveType; 4] = [
            MoveType::QueenPromotion,
            MoveType::KnightPromotion,
            MoveType::RookPromotion,
            MoveType::BishopPromotion,
        ];

        if tgt.is_promotion_square(side) {
            for promotion in PROMOTIONS {
                self.push(Move::new(src, tgt, promotion))
            }
        } else {
            self.push(Move::new(src, tgt, MoveType::Quiet))
        }
    }

    /// Push a pawn capture to the back of the movelist (do not use for enpassant)
    pub fn push_pawn_capture(&mut self, src: Square, tgt: Square, side: Color) {
        const PROMOTIONS: [MoveType; 4] = [
            MoveType::QueenCapPromo,
            MoveType::KnightCapPromo,
            MoveType::RookCapPromo,
            MoveType::BishopCapPromo,
        ];

        if tgt.is_promotion_square(side) {
            for promotion in PROMOTIONS {
                self.push(Move::new(src, tgt, promotion))
            }
        } else {
            self.push(Move::new(src, tgt, MoveType::Capture))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_movelist() {
        let mut l = MoveList::default();

        l.push_pawn_capture(Square::E2, Square::D3, Color::White);
        l.push_pawn_quiet(Square::E2, Square::E3, Color::White);

        assert_eq!(l.len, 2);
    }
}
