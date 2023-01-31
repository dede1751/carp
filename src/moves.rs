/// Implements move encoding and MoveList struct
use std::fmt;

use crate::{from, piece::*, square::*};

/// Indexed by color, rank at which each side's pawns promote
pub const PROMOTION_RANKS: [Rank; 2] = [Rank::Seventh, Rank::Second];

/// Indexed by color, rank at which each side's pawns start
pub const START_RANKS: [Rank; 2] = [Rank::Second, Rank::Seventh];

/// Moves, encoded in 32b
/// Smaller encoding can be achieved using only 2B (6b src, 6b tgt, 3b promotion, 1b extra) but it
/// makes the rest of the engine much less ergonomic. We only use the least significant 28 bits
///
///     0000 0000 0000 0000 0000 0011 1111    source             0x00003F     0
///     0000 0000 0000 0000 1111 1100 0000    target             0x000FC0     6
///     0000 0000 0000 1111 0000 0000 0000    piece              0x00F000    12
///     0000 0000 1111 0000 0000 0000 0000    captured piece     0x0F0000    16
///     0000 1111 0000 0000 0000 0000 0000    promoted piece     0x0F0000    20
///     0001 0000 0000 0000 0000 0000 0000    capture flag       0x100000    24
///     0010 0000 0000 0000 0000 0000 0000    double push flag   0x200000    25
///     0100 0000 0000 0000 0000 0000 0000    enpassant flag     0x400000    26
///     1000 0000 0000 0000 0000 0000 0000    castling flag      0x800000    27
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct Move(pub u32);

pub const NULL_MOVE: Move = Move(0);

// bit masks for the various parts of the move
const SRC: u32 = 0x0000003F;
const TGT: u32 = 0x00000FC0;
const PIECE: u32 = 0x0000F000;
const CAPTURE: u32 = 0x000F0000;
const PROMOTE: u32 = 0x00F00000;
const IS_CAP: u32 = 0x01000000;
const DOUBLE: u32 = 0x02000000;
const ENPASSANT: u32 = 0x04000000;
const CASTLE: u32 = 0x08000000;

/// Prints move in uci format
impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = format!("{}{}", self.get_src(), self.get_tgt());

        if self.get_promotion() != Piece::WP {
            write!(
                f,
                "{}{}",
                s,
                self.get_promotion().to_char().to_ascii_lowercase()
            )
        } else {
            write!(f, "{}", s)
        }
    }
}

impl Move {
    /// Init move through bitwise or of the various values shifted to correct place
    /// When not promoting, pass WP
    /// Flags are u32 boolean values (must be 0 or 1)
    pub const fn encode(
        src: Square,
        tgt: Square,
        piece: Piece,
        capture: Piece,
        promote: Piece,
        is_capture: u32,
        double_push: u32,
        en_passant: u32,
        castle: u32,
    ) -> Move {
        Move(
            (src as u32)
                | (tgt as u32) << 6
                | (piece as u32) << 12
                | (capture as u32) << 16
                | (promote as u32) << 20
                | is_capture << 24
                | double_push << 25
                | en_passant << 26
                | castle << 27,
        )
    }

    /// Returns the move source square
    pub const fn get_src(&self) -> Square {
        from!((self.0 & SRC) as u8, 63)
    }

    /// Returns the move target square
    pub const fn get_tgt(&self) -> Square {
        from!(((self.0 & TGT) >> 6) as u8, 63)
    }

    /// Returns the moving piece
    pub const fn get_piece(&self) -> Piece {
        from!(((self.0 & PIECE) >> 12) as u8, 63)
    }

    /// Returns the piece the pawn is promoting to
    pub const fn get_capture(&self) -> Piece {
        from!(((self.0 & CAPTURE) >> 16) as u8, 63)
    }

    /// Returns the piece the pawn is promoting to
    pub const fn get_promotion(&self) -> Piece {
        from!(((self.0 & PROMOTE) >> 20) as u8, 63)
    }

    /// Returns true if the move is a promotion
    pub const fn is_promotion(&self) -> bool {
        self.0 & PROMOTE != 0
    }

    /// Returns true if the move is a capture
    pub const fn is_capture(&self) -> bool {
        self.0 & IS_CAP != 0
    }

    /// Returns true if the move is a double pawn push
    pub const fn is_double_push(&self) -> bool {
        self.0 & DOUBLE != 0
    }

    /// Returns true if the move is an enpassant capture
    pub const fn is_enpassant(&self) -> bool {
        self.0 & ENPASSANT != 0
    }

    /// Returns true if the move is a castling move
    pub const fn is_castle(&self) -> bool {
        self.0 & CASTLE != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_constructor() {
        let m1 = Move::encode(
            Square::E2,
            Square::E4,
            Piece::WP,
            Piece::WP,
            Piece::WP,
            0,
            1,
            0,
            0,
        );
        let m2 = Move::encode(
            Square::H7,
            Square::G8,
            Piece::WP,
            Piece::BR,
            Piece::WQ,
            1,
            0,
            0,
            0,
        );

        assert!(m1.is_double_push());
        assert_eq!(m1.get_src(), Square::E2);
        assert_eq!(m1.get_tgt(), Square::E4);
        assert!(m2.is_capture());
        assert_eq!(m2.get_capture(), Piece::BR);
        assert_eq!(m2.get_promotion(), Piece::WQ);
    }
}
