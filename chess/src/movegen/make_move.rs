use crate::{
    bitboard::BitBoard,
    board::Board,
    castle::rook_castling_move,
    moves::{Move, MoveType},
    nnue::{NNUEState, OFF, ON},
};

impl Board {
    /// Makes (legal) move on the board
    /// Supplying illegal moves will lead to illegal board states.
    pub fn make_move(&self, m: Move) -> Board {
        let mut new = self.clone();
        let (src, tgt) = (m.get_src(), m.get_tgt());
        let piece = self.piece_at(src); // must exist
        let move_type = m.get_type();
        let capture = move_type.is_capture();

        // Remove moving piece and reset halfmoves
        new.remove_piece(src);
        if capture || piece.is_pawn() {
            new.halfmoves = 0
        } else {
            new.halfmoves += 1;
        }

        // Handle pieces affected by the move (captures/castles..)
        if move_type == MoveType::EnPassant {
            new.remove_piece(tgt.forward(!self.side));
        } else if capture {
            new.remove_piece(tgt);
        } else if move_type == MoveType::Castle {
            let rook = self.side.rook();
            let (rook_src, rook_tgt) = rook_castling_move(tgt);

            new.remove_piece(rook_src);
            new.set_piece(rook, rook_tgt);
        }

        // Move the piece to the new square
        if move_type.is_promotion() {
            new.set_piece(move_type.get_promotion(self.side), tgt);
        } else {
            new.set_piece(piece, tgt);
        }

        // Handle enpassant
        if let Some(square) = self.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        // Handle double push
        if move_type == MoveType::DoublePush {
            let ep_tgt = src.forward(self.side);

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        // Handle castling rights
        let new_rights = self.castling_rights.update(src, tgt);
        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        new.side = !self.side;
        new.hash.toggle_side();
        new.checkers = new.checkers();

        new
    }

    /// Make move with NNUE accumulator increments.
    pub fn make_move_nnue(&self, m: Move, nnue_state: &mut Box<NNUEState>) -> Board {
        let mut new = self.clone();
        let (src, tgt) = (m.get_src(), m.get_tgt());
        let piece = self.piece_at(src);
        let move_type = m.get_type();
        let capture = move_type.is_capture();

        // add new accumulator
        nnue_state.push();

        new.remove_piece(src);
        if capture || piece.is_pawn() {
            new.halfmoves = 0
        } else {
            new.halfmoves += 1;
        }

        if move_type == MoveType::EnPassant {
            let ep_target = tgt.forward(!self.side);

            new.remove_piece(ep_target);
            nnue_state.manual_update::<OFF>((!self.side).pawn(), ep_target);
        } else if capture {
            new.remove_piece(tgt);
            nnue_state.manual_update::<OFF>(self.piece_at(tgt), tgt);
        } else if move_type == MoveType::Castle {
            let rook = self.side.rook();
            let (rook_src, rook_tgt) = rook_castling_move(tgt);

            new.remove_piece(rook_src);
            new.set_piece(rook, rook_tgt);
            nnue_state.move_update(rook, rook_src, rook_tgt);
        }

        if move_type.is_promotion() {
            let promotion = move_type.get_promotion(self.side);

            new.set_piece(promotion, tgt);
            nnue_state.manual_update::<OFF>(piece, src);
            nnue_state.manual_update::<ON>(promotion, tgt);
        } else {
            new.set_piece(piece, tgt);
            nnue_state.move_update(piece, src, tgt);
        }

        if let Some(square) = self.en_passant {
            new.en_passant = None;
            new.hash.toggle_ep(square);
        }

        if move_type == MoveType::DoublePush {
            let ep_tgt = src.forward(self.side);

            new.en_passant = Some(ep_tgt);
            new.hash.toggle_ep(ep_tgt);
        }

        let new_rights = self.castling_rights.update(src, tgt);
        new.castling_rights = new_rights;
        new.hash.swap_castle(self.castling_rights, new_rights);

        new.side = !self.side;
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
