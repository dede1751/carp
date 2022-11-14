//! # Implements move encoding and MoveList struct

use std::fmt;

use crate::square::*;
use crate::piece::*;

///      Move -- Encodes move in a single 4B word
/// Smaller encoding can be achieved using only 2B (6b src, 6b tgt, 3b promotion, 1b extra) but it
/// makes the rest of the engine much less ergonomic. We only use the least significant 24 bits
/// 
///     0000 0000 0000 0000 0011 1111    source             0x00003F     0
///     0000 0000 0000 1111 1100 0000    target             0x000FC0     6
///     0000 0000 1111 0000 0000 0000    piece              0x00F000    12
///     0000 1111 0000 0000 0000 0000    promoted piece     0x0F0000    16
///     0001 0000 0000 0000 0000 0000    capture flag       0x100000    20
///     0010 0000 0000 0000 0000 0000    double push flag   0x200000    21
///     0100 0000 0000 0000 0000 0000    enpassant flag     0x400000    22
///     1000 0000 0000 0000 0000 0000    castling flag      0x800000    23
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct Move (u32);

const SRC        : u32 = 0x00003F;
const TGT        : u32 = 0x000FC0;
const PIECE      : u32 = 0x00F000;
const PROMOTE    : u32 = 0x0F0000;
const CAPTURE    : u32 = 0x100000;
const DOUBLE_PUSH: u32 = 0x200000;
const ENPASSANT  : u32 = 0x400000;
const CASTLE     : u32 = 0x800000;

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let promotion: char = match self.get_promotion() {
            Piece::WP => '-',
            p => p.to_unicode()
        };

        write!(
            f,
"\n Src | Tgt | Piece | Promote | Capture | DoublePush | En Passant |  Castle
----------------------------------------------------------------------------
  {} | {}  |   {}   |    {}    |  {}   |   {}    |    {}   |  {}\n",
            self.get_src(),
            self.get_tgt(),
            self.get_piece().to_unicode(),
            promotion,
            self.is_capture() as bool,
            self.is_double_push() as bool,
            self.is_enpassant() as bool,
            self.is_castle() as bool
        )
    }
}

impl Move {
    /// Init move through bitwise or of the various values shifted to correct place
    /// 
    /// When not promoting, pass WP
    /// Flags are u32 boolean values (must be 0 or 1)
    #[inline]
    pub fn encode(
        src: Square, tgt: Square, piece: Piece, promote: Piece,         // values
        capture: u32, double_push: u32, en_passant: u32, castle: u32    // flags
    ) -> Move {
        Move(
            src.index() as u32 | (tgt.index() << 6) as u32 |
            (piece.index() << 12) as u32 | (promote.index() << 16) as u32 |
            capture << 20 | double_push << 21 | en_passant << 22 | castle << 23
        )
    }

    /// Returns the move source square
    #[inline]
    pub fn get_src(&self) -> Square {
        Square::from_index((self.0 & SRC) as usize)
    }

    /// Returns the move target square
    #[inline]
    pub fn get_tgt(&self) -> Square {
        Square::from_index(((self.0 & TGT) >> 6) as usize)
    }

    /// Returns the moving piece
    #[inline]
    pub fn get_piece(&self) -> Piece {
        Piece::from_index(((self.0 & PIECE) >> 12) as usize)
    }

    /// Returns the piece the pawn is promoting to
    #[inline]
    pub fn get_promotion(&self) -> Piece {
        Piece::from_index(((self.0 & PROMOTE) >> 16) as usize)
    }

    /// Returns true if the move is a capture
    #[inline]
    pub fn is_capture(&self) -> bool {
        self.0 & CAPTURE != 0
    }

    /// Returns true if the move is a double pawn push
    #[inline]
    pub fn is_double_push(&self) -> bool {
        self.0 & DOUBLE_PUSH != 0
    }

    /// Returns true if the move is an enpassant capture
    #[inline]
    pub fn is_enpassant(&self) -> bool {
        self.0 & ENPASSANT != 0
    }

    /// Returns true if the move is a castling move
    #[inline]
    pub fn is_castle(&self) -> bool {
        self.0 & CASTLE  != 0
    }
}


///     MoveList -- Allows simple separation of captures and quiets for quiescent search
pub struct MoveList {
    pub captures: Vec<Move>,
    pub quiets  : Vec<Move>,
}

impl IntoIterator for MoveList {
    type Item = Move;
    type IntoIter = std::iter::Chain<
        std::vec::IntoIter<Self::Item>, std::vec::IntoIter<Self::Item>
    >;

    // chains the two move lists
    fn into_iter(self) -> Self::IntoIter {
        self.captures.into_iter().chain(self.quiets.into_iter())
    }
}

impl MoveList {
    pub fn new() -> MoveList {
        MoveList {
            captures: Vec::new(),
            quiets: Vec::new(),
        }
    }

    /// Returns total length of the move list
    #[inline]
    pub fn len(&self) -> usize {
        self.captures.len() + self.quiets.len()
    }

    /// Initialize and add a capture move to the capture vector
    #[inline]
    pub fn add_capture(
        &mut self,
        src: Square, tgt: Square, piece: Piece, promote: Piece,
        en_passant: u32
    ) {
        self.captures.push(
            Move::encode(src, tgt, piece, promote, 1, 0, en_passant, 0)
        )
    }

    /// Initialize and add a quiet move to the quiet vector
    #[inline]
    pub fn add_quiet(
        &mut self,
        src: Square, tgt: Square, piece: Piece, promote: Piece,
        double_push: u32, castle: u32
    ) {
        self.quiets.push(
            Move::encode(src, tgt, piece, promote, 0, double_push, 0, castle)
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_move_constructor(){
        let m1: Move = Move::encode(
            Square::E2, Square::E4, Piece::WP, Piece::WP, 0, 1, 0, 0);
        let m2: Move = Move::encode(Square::H7, Square::G8, Piece::WP, Piece::WQ, 1, 0, 0, 0);

        assert!(m1.is_double_push());
        assert_eq!(m1.get_src(), Square::E2);
        assert_eq!(m1.get_tgt(), Square::E4);
        assert!(m2.is_capture());
        assert_eq!(m2.get_promotion(), Piece::WQ);
    }

    #[test]
    fn test_movelist() {
        let mut l: MoveList = MoveList::new();

        l.add_capture(Square::E2, Square::D3, Piece::WP, Piece::WP, 0);
        l.add_quiet(Square::E2, Square::E4, Piece::WP, Piece::WP, 1, 0);

        let full_list: Vec<Move> = l.into_iter().collect();
        assert_eq!(full_list.len(), 2);
    }
}