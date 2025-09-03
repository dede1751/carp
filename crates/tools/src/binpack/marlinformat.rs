use chess::{
    board::Board,
    piece::Color,
    square::{File, Rank},
};
use engine::position::GameResult;

use super::endian;

const UNMOVED_ROOK: u8 = 6; // one higher than the max piecetype enum value

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct PackedBoard {
    pub occupancy: endian::U64Le,
    pub pieces: endian::U4Array32,
    pub stm_ep_square: u8,
    pub halfmove_clock: u8,
    pub fullmove_number: endian::U16Le,
    pub eval: endian::I16Le,
    pub wdl: u8,
    pub extra: u8,
}

impl PackedBoard {
    pub fn pack(board: &Board) -> Self {
        // Marlinformat uses A1=0, we use A8=0, so we have to flip a couple of things
        let occupancy = board.occupancy().flipv();

        let mut pieces = endian::U4Array32::default();
        for (i, sq) in occupancy.enumerate() {
            let sq = sq.flipv(); // need to flip back
            let piece = board.piece_at(sq);
            let color = piece.color();

            let first_rank = (sq.rank() == Rank::First && color == Color::White)
                || (sq.rank() == Rank::Eight && color == Color::Black);
            let can_castle = (sq.file() == File::A && board.castling_rights.has_queenside(color))
                || (sq.file() == File::H && board.castling_rights.has_kingside(color));

            let piece_code = if piece.is_rook() && first_rank && can_castle {
                UNMOVED_ROOK
            } else {
                piece.inner()
            };
            pieces.set(i, piece_code | (color.inner() << 3));
        }

        // Note that we ignore fullmove_number/wdl/eval/extra, which are irrelevant for Viriformat/bullet
        let ep_square = board.en_passant.map_or(64, |sq| sq.flipv().inner());
        Self {
            occupancy: endian::U64Le::new(occupancy.inner()),
            pieces,
            stm_ep_square: (board.side.inner() << 7) | ep_square,
            halfmove_clock: board.halfmoves as u8,
            fullmove_number: endian::U16Le::new(0),
            wdl: 0,
            eval: endian::I16Le::new(0),
            extra: 0,
        }
    }

    pub fn set_result(&mut self, result: GameResult) {
        self.wdl = result.as_packed_u8();
    }

    pub const fn as_bytes(self) -> [u8; std::mem::size_of::<Self>()] {
        // SAFETY: PackedBoard is entirely integer types, and so can be safely transmuted into bytes.
        unsafe { std::mem::transmute(self) }
    }
}
