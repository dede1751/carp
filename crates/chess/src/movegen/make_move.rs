use crate::{
    bitboard::BitBoard,
    board::Board,
    castle::rook_castling_move,
    moves::{Move, MoveType},
    piece::{Color, PieceType},
    square::Square,
};

pub type Feature = (PieceType, Color, Square);

pub const ON: bool = true;
pub const OFF: bool = false;
pub trait FeatureUpdate {
    fn update_weights<const ON: bool>(&mut self, feat: Feature);
    fn add_sub_weights(&mut self, add: Feature, sub: Feature);
}

impl Board {
    /// Makes (legal) move on the board
    /// Supplying illegal moves will lead to illegal board states.
    pub fn make_move(&self, m: Move) -> Board {
        let mut new = self.clone();

        let us = self.side;
        let them = !self.side;
        let (src, tgt) = (m.get_src(), m.get_tgt());
        let piece = self.piece_type_at(src); // must exist
        let move_type = m.get_type();
        let capture = move_type.is_capture();

        // Remove moving piece and reset halfmoves
        new.pop_piece(piece, us, src);
        if capture || piece == PieceType::Pawn {
            new.halfmoves = 0;
        } else {
            new.halfmoves += 1;
        }

        if capture {
            let (tgt_piece, tgt_sq) = if move_type == MoveType::EnPassant {
                (PieceType::Pawn, tgt.forward(them))
            } else {
                (self.piece_type_at(tgt), tgt)
            };

            new.pop_piece(tgt_piece, them, tgt_sq);
        } else if move_type == MoveType::Castle {
            let rook = PieceType::Rook;
            let (rook_src, rook_tgt) = rook_castling_move(tgt);
            new.pop_piece(rook, us, rook_src);
            new.set_piece(rook, us, rook_tgt);
        }

        // Move the piece to the new square
        let new_piece = if move_type.is_promotion() {
            move_type.get_promotion()
        } else {
            piece
        };
        new.set_piece(new_piece, us, tgt);

        // Handle enpassant
        if let Some(square) = self.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        // Handle double push
        if move_type == MoveType::DoublePush {
            let ep_tgt = src.forward(us);

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        // Handle castling rights
        let new_rights = self.castling_rights.update(src, tgt);
        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        new.side = them;
        new.hash.toggle_side();
        new.checkers = new.checkers();

        new
    }

    /// Make move with NNUE accumulator increments.
    pub fn make_move_nnue<T: FeatureUpdate>(&self, m: Move, acc: &mut T) -> Board {
        let mut new = self.clone();

        let us = self.side;
        let them = !self.side;
        let (src, tgt) = (m.get_src(), m.get_tgt());
        let piece = self.piece_type_at(src);
        let move_type = m.get_type();
        let capture = move_type.is_capture();

        new.pop_piece(piece, us, src);
        if capture || piece == PieceType::Pawn {
            new.halfmoves = 0
        } else {
            new.halfmoves += 1;
        }

        if capture {
            let (tgt_piece, tgt_sq) = if move_type == MoveType::EnPassant {
                (PieceType::Pawn, tgt.forward(them))
            } else {
                (self.piece_type_at(tgt), tgt)
            };

            new.pop_piece(tgt_piece, them, tgt_sq);
            acc.update_weights::<OFF>((tgt_piece, them, tgt_sq));
        } else if move_type == MoveType::Castle {
            let rook = PieceType::Rook;
            let (rook_src, rook_tgt) = rook_castling_move(tgt);
            new.pop_piece(rook, us, rook_src);
            new.set_piece(rook, us, rook_tgt);
            acc.add_sub_weights((rook, us, rook_tgt), (rook, us, rook_src));
        }

        let new_piece = if move_type.is_promotion() {
            move_type.get_promotion()
        } else {
            piece
        };
        new.set_piece(new_piece, us, tgt);
        acc.add_sub_weights((new_piece, us, tgt), (piece, us, src));

        if let Some(square) = self.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        if move_type == MoveType::DoublePush {
            let ep_tgt = src.forward(us);

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        let new_rights = self.castling_rights.update(src, tgt);
        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        new.side = them;
        new.hash.toggle_side();
        new.checkers = new.checkers();

        new
    }

    /// Makes the null move on the board, giving the turn to the opponent
    /// Calling this function when in check breaks the game state.
    pub fn make_null(&self) -> Board {
        let mut new = self.clone();
        new.side = !self.side;
        new.hash.toggle_side();

        new.en_passant = None;
        if let Some(square) = self.en_passant {
            new.hash.toggle_ep(square);
        }
        new.checkers = BitBoard::EMPTY;

        new
    }
}
