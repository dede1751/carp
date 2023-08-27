use std::fmt;

use crate::{piece::*, square::*, transmute_enum};

/// Moves, encoded in 16b (encoding scheme is from Midnight by Archi)
///
///     0000 0000 0011 1111    source       0x003F     0
///     0000 1111 1100 0000    target       0x0FC0     6
///     1111 0000 0000 0000    move type    0x7000    12
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Default, Hash)]
pub struct Move(pub u16);

pub const NULL_MOVE: Move = Move(0);

/// Flag for the type of move, fits in 4b
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum MoveType {
    Quiet = 0b0000,
    Castle = 0b0001,
    DoublePush = 0b0010,

    // Promotions have 3rd bit set
    KnightPromotion = 0b0100,
    BishopPromotion = 0b0101,
    RookPromotion = 0b0110,
    QueenPromotion = 0b0111,

    // Captures have 4th bit set
    Capture = 0b1000,
    EnPassant = 0b1001,

    KnightCapPromo = 0b1100,
    BishopCapPromo = 0b1101,
    RookCapPromo = 0b1110,
    QueenCapPromo = 0b1111,
}

impl MoveType {
    /// Returns true if the move is a promotion
    pub const fn is_promotion(self) -> bool {
        self as usize & 0b0100 != 0
    }

    /// Returns true if the move is an underpromotion.
    pub const fn is_underpromotion(self) -> bool {
        self.is_promotion() && self as usize & 0b0111 != 0b0111
    }

    /// Returns true if the move is a capture (include enpassant)
    pub const fn is_capture(self) -> bool {
        self as usize & 0b1000 != 0
    }

    /// Returns true if the move is not a capture or promotion
    pub const fn is_quiet(self) -> bool {
        self as usize & 0b1100 == 0
    }

    /// Returns the promotion piece of the given color. MoveType must be a promotion.
    pub const fn get_promotion(self, side: Color) -> Piece {
        const PROMO_MASK: usize = 0b0011;
        const PROMO_PIECES: [[Piece; 4]; 2] = [
            [Piece::WN, Piece::WB, Piece::WR, Piece::WQ],
            [Piece::BN, Piece::BB, Piece::BR, Piece::BQ],
        ];

        PROMO_PIECES[side as usize][self as usize & PROMO_MASK]
    }
}

// bit masks for the various parts of the move
const SRC: u16 = 0b0000_0000_0011_1111;
const TGT: u16 = 0b0000_1111_1100_0000;
const TYPE: u16 = 0b1111_0000_0000_0000;

/// Prints move in uci format
impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = format!("{}{}", self.get_src(), self.get_tgt());
        let move_type = self.get_type();

        if move_type.is_promotion() {
            write!(
                f,
                "{}{}",
                s,
                move_type.get_promotion(Color::Black).to_char()
            )
        } else {
            write!(f, "{s}")
        }
    }
}

impl Move {
    /// Init move through bitwise or of the various values shifted to correct place
    pub const fn new(src: Square, tgt: Square, move_type: MoveType) -> Move {
        Move((src as u16) | (tgt as u16) << 6 | (move_type as u16) << 12)
    }

    /// Returns the move source square
    pub const fn get_src(self) -> Square {
        transmute_enum!((self.0 & SRC) as u8, 63)
    }

    /// Returns the move target square
    pub const fn get_tgt(self) -> Square {
        transmute_enum!(((self.0 & TGT) >> 6) as u8, 63)
    }

    /// Returns the move type flag
    pub const fn get_type(self) -> MoveType {
        transmute_enum!(((self.0 & TYPE) >> 12) as u8, 15)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::Board;

    #[test]
    fn test_move_constructor() {
        let b: Board = "R2bk3/5p2/4r1B1/1Q6/8/4Q3/4R3/2K5 b - - 0 1"
            .parse()
            .unwrap();

        println!("{}", b);

        let m1 = Move::new(Square::F7, Square::G6, MoveType::Capture);
        let m2 = Move::new(Square::E8, Square::E7, MoveType::Quiet);

        assert_eq!(m1.get_src(), Square::F7);
        assert_eq!(m1.get_tgt(), Square::G6);
        assert_eq!(m1.get_type(), MoveType::Capture);

        assert_eq!(b.piece_at(Square::F7), Piece::BP);
        assert_eq!(b.piece_at(Square::G6), Piece::WB);

        assert_eq!(m2.get_src(), Square::E8);
        assert_eq!(m2.get_tgt(), Square::E7);
        assert_eq!(m2.get_type(), MoveType::Quiet);
    }
}
