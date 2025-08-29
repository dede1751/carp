use super::{
    network::{HIDDEN, MODEL},
    Align64,
};
use crate::search_params::MAX_DEPTH;
pub use chess::movegen::make_move::AccumulatorStack; // re-export
use chess::movegen::make_move::ON;
use chess::{board::Board, piece::Piece, square::Square};
use std::alloc;

pub type SideAccumulator = Align64<[i16; HIDDEN]>;

/// Accumulators contain the efficiently updated hidden layer values
/// Each accumulator is perspective, hence both contains the white and black pov
#[derive(Clone, Copy, Debug)]
pub struct Accumulator {
    pub white: SideAccumulator,
    pub black: SideAccumulator,
}

impl Default for Accumulator {
    fn default() -> Self {
        Self {
            white: MODEL.feature_bias,
            black: MODEL.feature_bias,
        }
    }
}

/// Returns white and black feature index for given feature
const fn nnue_index(piece: Piece, sq: Square) -> (usize, usize) {
    const COLOR_STRIDE: usize = 64 * 6;
    const PIECE_STRIDE: usize = 64;
    let p = (piece as usize) / 2;
    let c = piece.color() as usize;

    let white_idx = c * COLOR_STRIDE + p * PIECE_STRIDE + sq.flipv() as usize;
    let black_idx = (1 ^ c) * COLOR_STRIDE + p * PIECE_STRIDE + sq as usize;

    (white_idx * HIDDEN, black_idx * HIDDEN)
}

/// NNUEState is simply a stack of accumulators, updated along the search tree
#[derive(Debug, Clone)]
pub struct NNUEState {
    pub(super) accumulator_stack: [Accumulator; MAX_DEPTH + 1],
    pub(super) current_acc: usize,
}

impl NNUEState {
    /// Inits nnue state from a board
    /// To be able to run debug builds, heap is allocated manually
    pub fn from_board(board: &Board) -> Box<Self> {
        let mut boxed: Box<Self> = unsafe {
            let layout = alloc::Layout::new::<Self>();
            let ptr = alloc::alloc_zeroed(layout);
            if ptr.is_null() {
                alloc::handle_alloc_error(layout);
            }
            Box::from_raw(ptr.cast())
        };

        // init with feature biases and add in all features of the board
        boxed.accumulator_stack[0] = Accumulator::default();
        for sq in board.occupancy() {
            boxed.manual_update::<ON>(board.piece_at(sq), sq);
        }

        boxed
    }

    /// Refresh the accumulator stack to the given board
    pub fn refresh(&mut self, board: &Board) {
        // reset the accumulator stack
        self.current_acc = 0;
        self.accumulator_stack[self.current_acc] = Accumulator::default();

        // update the first accumulator
        for piece in Piece::ALL {
            for sq in board.piece_occupancy(piece) {
                self.manual_update::<ON>(piece, sq);
            }
        }
    }
}

impl AccumulatorStack for NNUEState {
    /// Add a new accumulator to the stack by copying the previous top
    fn push(&mut self) {
        self.accumulator_stack[self.current_acc + 1] = self.accumulator_stack[self.current_acc];
        self.current_acc += 1;
    }

    /// Pop the top off the accumulator stack
    fn pop(&mut self) {
        self.current_acc -= 1;
    }

    /// Manually turn on or off the single given feature
    fn manual_update<const ON: bool>(&mut self, piece: Piece, sq: Square) {
        self.accumulator_stack[self.current_acc].update_weights::<ON>(nnue_index(piece, sq));
    }

    /// Efficiently update accumulator for a quiet move (that is, only changes from/to features)
    fn move_update(&mut self, piece: Piece, from: Square, to: Square) {
        let from_idx = nnue_index(piece, from);
        let to_idx = nnue_index(piece, to);

        self.accumulator_stack[self.current_acc].add_sub_weights(from_idx, to_idx);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chess::movegen::make_move::OFF;

    #[test]
    fn test_nnue_stack() {
        let b = Board::default();
        let s1 = NNUEState::from_board(&b);
        let mut s2 = NNUEState::from_board(&b);

        s2.push();
        s2.pop();

        for i in 0..HIDDEN {
            assert_eq!(
                s1.accumulator_stack[0].white[i],
                s2.accumulator_stack[0].white[i]
            );
            assert_eq!(
                s1.accumulator_stack[0].black[i],
                s2.accumulator_stack[0].black[i]
            );
        }
        assert_eq!(s1.current_acc, s2.current_acc);
    }

    #[test]
    fn test_nnue_index() {
        let idx1 = nnue_index(Piece::WP, Square::A8);
        let idx2 = nnue_index(Piece::WP, Square::H1);
        let idx3 = nnue_index(Piece::BP, Square::A1);
        let idx4 = nnue_index(Piece::WK, Square::E1);

        assert_eq!(idx1, (HIDDEN * 56, HIDDEN * 384));
        assert_eq!(idx2, (HIDDEN * 7, HIDDEN * 447));
        assert_eq!(idx3, (HIDDEN * 384, HIDDEN * 56));
        assert_eq!(idx4, (HIDDEN * 324, HIDDEN * 764));
    }

    #[test]
    fn test_manual_update() {
        let b: Board = Board::default();
        let mut s1 = NNUEState::from_board(&b);

        let old_acc = s1.accumulator_stack[0];

        s1.manual_update::<ON>(Piece::WP, Square::A3);
        s1.manual_update::<OFF>(Piece::WP, Square::A3);

        for i in 0..HIDDEN {
            assert_eq!(old_acc.white[i], s1.accumulator_stack[0].white[i]);
            assert_eq!(old_acc.black[i], s1.accumulator_stack[0].black[i]);
        }
    }

    #[test]
    fn test_incremental_updates() {
        let b1: Board = Board::default();
        let m = b1.find_move("e2e4").unwrap();
        let b2: Board = b1.make_move(m);

        let mut s1 = NNUEState::from_board(&b1);
        let s2 = NNUEState::from_board(&b2);

        s1.move_update(b1.piece_at(m.get_src()), m.get_src(), m.get_tgt());

        for i in 0..HIDDEN {
            assert_eq!(
                s1.accumulator_stack[0].white[i],
                s2.accumulator_stack[0].white[i]
            );
            assert_eq!(
                s1.accumulator_stack[0].black[i],
                s2.accumulator_stack[0].black[i]
            );
        }
    }
}
