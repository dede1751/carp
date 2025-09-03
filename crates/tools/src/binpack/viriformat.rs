use super::endian;
use std::mem::size_of;

use super::marlinformat::PackedBoard;
use chess::{
    board::Board,
    castle::rook_castling_move,
    moves::{Move, MoveType},
};
use engine::position::GameResult;

const SEQUENCE_ELEM_SIZE: usize = size_of::<Move>() + size_of::<endian::I16Le>();
const NULL_TERMINATOR: [u8; SEQUENCE_ELEM_SIZE] = [0; SEQUENCE_ELEM_SIZE];

const CARP_TO_VIRI: [u16; 16] = [
    0b0000, // Quiet
    0b1000, // Castle
    0b0000, // DoublePush
    0b0000, //      ---
    0b1100, // KnightPromotion
    0b1101, // BishopPromotion
    0b1110, // RookPromotion
    0b1111, // QueenPromotion
    0b0000, // Capture
    0b0100, // EnPassant
    0b0000, //      ---
    0b0000, //      ---
    0b1100, // KnightCapPromo
    0b1101, // BishopCapPromo
    0b1110, // RookCapPromo
    0b1111, // QueenCapPromo
];

/// We have to convert quite a lot of things:
///  - We must horizontally flip the squares:
///    VIRI: A1= 0, H1= 7, A8=56, H8=63
///    CARP: A1=56, H1=63, A8= 0, H8= 7
///  - Viri's movetype encodes captures/doublepushes differently, need to remap.
///  - Viri's castling moves are encoded as king-takes-rook for DFRC support.
fn to_viriformat(m: Move) -> u16 {
    let tgt_sq = if m.get_type() == MoveType::Castle {
        rook_castling_move(m.get_tgt()).0
    } else {
        m.get_tgt()
    };
    let src = m.get_src().flipv().inner() as u16;
    let tgt = tgt_sq.flipv().inner() as u16;
    let type_flag = CARP_TO_VIRI[m.get_type() as usize];

    (type_flag << 12) | (tgt << 6) | src
}

pub struct ViriformatGame {
    startpos: PackedBoard,
    moves: Vec<(endian::U16Le, endian::I16Le)>,
}

impl ViriformatGame {
    pub fn new() -> Self {
        Self {
            startpos: PackedBoard::default(),
            moves: Vec::with_capacity(256),
        }
    }

    pub fn len(&self) -> usize {
        self.moves.len() + 1
    }

    pub fn start(&mut self, board: &Board) {
        self.startpos = PackedBoard::pack(board);
        self.moves.clear();
    }

    pub fn push_move(&mut self, mv: Move, eval: i32) {
        let mv = endian::U16Le::new(to_viriformat(mv));
        let eval = endian::I16Le::new(eval as i16);
        self.moves.push((mv, eval));
    }

    pub fn set_result(&mut self, result: GameResult) {
        self.startpos.set_result(result);
    }

    pub fn write_bytes(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        writer.write_all(&self.startpos.as_bytes())?;
        for (mv, eval) in &self.moves {
            writer.write_all(&mv.get().to_le_bytes())?;
            writer.write_all(&eval.get().to_le_bytes())?;
        }
        writer.write_all(&NULL_TERMINATOR)?;
        Ok(())
    }
}
