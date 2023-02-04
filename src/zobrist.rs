use crate::board::*;
use crate::castle::*;
use crate::piece::*;
use crate::square::*;
use crate::tables::*;

/// Zobrist hash, an incremental hash for a board position using random keys (in constants mod)
///
/// Didactic note:
/// Zobrist hashes for two identical positions are the same ONLY if obtained through any combination
/// of toggles from the SAME state. Given position A and B, if they both lead to C through m1..mn,
/// ZH(C(A)) == ZH(C(B))   <=>     ZH(B) is obtained from ZH(A) through some sequence of moves.
///
/// Building ZH(A) and ZH(B) independently by summing material score WILL NOT produce the same hash
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
pub struct ZHash(pub u64);

pub const NULL_HASH: ZHash = ZHash(0);

impl ZHash {
    pub fn new(board: &Board) -> ZHash {
        let mut hash: ZHash = ZHash(0);

        for piece in ALL_PIECES {
            for square in board.pieces[piece as usize] {
                hash.toggle_piece(piece, square);
            }
        }

        if let Some(square) = board.en_passant {
            hash.toggle_ep(square);
        }

        hash.toggle_castle(board.castling_rights);
        if board.side == Color::White {
            hash.toggle_side();
        }

        hash
    }

    /// Toggle when piece moves to/from square
    pub fn toggle_piece(&mut self, piece: Piece, square: Square) {
        self.0 ^= PIECE_KEYS[piece as usize][square as usize];
    }

    /// Toggles source and target squares
    pub fn move_piece(&mut self, piece: Piece, from: Square, to: Square) {
        self.0 ^= PIECE_KEYS[piece as usize][from as usize];
        self.0 ^= PIECE_KEYS[piece as usize][to as usize];
    }

    /// Toggle the enpassant square
    pub fn toggle_ep(&mut self, square: Square) {
        self.0 ^= EP_KEYS[square as usize];
    }

    /// Toggles the given castling index
    pub fn toggle_castle(&mut self, castle: CastlingRights) {
        self.0 ^= CASTLE_KEYS[castle.index()];
    }

    /// Toggles out old castle rights and toggles in new
    pub fn swap_castle(&mut self, old_castle: CastlingRights, new_castle: CastlingRights) {
        self.0 ^= CASTLE_KEYS[old_castle.index()];
        self.0 ^= CASTLE_KEYS[new_castle.index()];
    }

    /// Toggles side to move
    pub fn toggle_side(&mut self) {
        self.0 ^= SIDE_KEY;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{moves::Move, position::Position};

    #[test]
    pub fn test_hash_init() {
        let b1: Board = Board::default();
        let b2: Board = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
            .parse()
            .unwrap();

        assert_eq!(ZHash::new(&b1), ZHash(11231077536533049824)); // correct start hash
        assert_eq!(ZHash::new(&b2), b2.hash); // try_from() builds hash correctly
    }

    #[test]
    pub fn test_hash_castle() {
        // testing white king castling kingside
        let b1: Board = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
            .parse()
            .unwrap();
        let m = Move::encode(
            Square::E1,
            Square::G1,
            Piece::WK,
            Piece::WP,
            Piece::WP,
            0,
            0,
            0,
            1,
        );
        let b2 = b1.make_move(m);

        println!("{}\n{}", b1, b2);

        let mut z1 = b1.hash;
        z1.toggle_piece(Piece::WK, Square::E1);
        z1.toggle_piece(Piece::WK, Square::G1);
        z1.toggle_piece(Piece::WR, Square::H1);
        z1.toggle_piece(Piece::WR, Square::F1);

        let old_rights: CastlingRights = "KQkq".parse().unwrap();
        let new_rights: CastlingRights = "kq".parse().unwrap();
        z1.swap_castle(old_rights, new_rights);
        z1.toggle_side();

        // z1 is the same as we obtained through incremental hash updates in make move
        assert_eq!(z1, b2.hash);
    }

    #[test]
    pub fn test_hash_enpassant() {
        // testing white pawn capturing en passant
        let b1: Board = "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"
            .parse()
            .unwrap();
        let m = Move::encode(
            Square::F5,
            Square::E6,
            Piece::WP,
            Piece::BP,
            Piece::WP,
            1,
            0,
            1,
            0,
        );
        let b2 = b1.make_move(m);

        let mut z1 = b1.hash;
        z1.toggle_piece(Piece::WP, Square::F5);
        z1.toggle_piece(Piece::WP, Square::E6);
        z1.toggle_piece(Piece::BP, Square::E5);

        z1.toggle_ep(Square::E6);
        z1.toggle_side();

        // z1 is the same as we obtained through incremental hash updates in make move
        assert_eq!(z1, b2.hash);
    }

    #[test]
    pub fn test_hash_null() {
        // testing null move
        let mut pos: Position =
            "fen rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"
                .parse()
                .unwrap();
        let mut z1 = pos.board.hash;

        z1.toggle_ep(Square::E6);
        z1.toggle_side();
        pos.make_null();

        // z1 is the same as we obtained through incremental hash updates in make move
        assert_eq!(z1, pos.board.hash);
    }
}
