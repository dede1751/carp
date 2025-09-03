use super::{
    Align64,
    network::{HIDDEN, MODEL},
};
use crate::search_params::MAX_DEPTH;
use chess::movegen::make_move::{Feature, FeatureUpdate, ON};
use chess::{board::Board, piece::Piece};
use std::alloc;

pub type SideAccumulator = Align64<[i16; HIDDEN]>;

/// Accumulators contain the efficiently updated hidden layer values
/// Each accumulator is perspective, hence both contains the white and black pov
#[derive(Clone, Copy, Debug)]
pub struct Accumulator {
    pub(super) white: SideAccumulator,
    pub(super) black: SideAccumulator,
}

impl Default for Accumulator {
    fn default() -> Self {
        Self {
            white: MODEL.feature_bias,
            black: MODEL.feature_bias,
        }
    }
}

impl Accumulator {
    /// Returns white and black feature index for given feature
    #[inline(always)]
    const fn idx(feat: Feature) -> (usize, usize) {
        const COLOR_STRIDE: usize = 64 * 6;
        const PIECE_STRIDE: usize = 64;
        let p = feat.0.type_index();
        let c = feat.0.color().index();

        let white_idx = c * COLOR_STRIDE + p * PIECE_STRIDE + feat.1.flipv().index();
        let black_idx = (1 ^ c) * COLOR_STRIDE + p * PIECE_STRIDE + feat.1.index();

        (white_idx * HIDDEN, black_idx * HIDDEN)
    }
}

impl FeatureUpdate for Accumulator {
    /// Updates weights for a single feature, either turning them on or off
    #[inline(always)]
    fn update_weights<const ON: bool>(&mut self, feat: Feature) {
        #[inline(always)]
        fn update<const ON: bool>(acc: &mut SideAccumulator, idx: usize) {
            let zip = acc
                .iter_mut()
                .zip(MODEL.feature_weights[idx..idx + HIDDEN].iter());

            for (acc_val, &weight) in zip {
                if ON {
                    *acc_val += weight;
                } else {
                    *acc_val -= weight;
                }
            }
        }

        let idx = Self::idx(feat);
        update::<ON>(&mut self.white, idx.0);
        update::<ON>(&mut self.black, idx.1);
    }

    /// Update accumulator for a quiet move.
    /// Adds in features for the destination and removes the features of the source
    #[inline(always)]
    fn add_sub_weights(&mut self, add: Feature, sub: Feature) {
        #[inline(always)]
        fn add_sub(acc: &mut SideAccumulator, add: usize, sub: usize) {
            let zip = acc.iter_mut().zip(
                MODEL.feature_weights[add..add + HIDDEN]
                    .iter()
                    .zip(MODEL.feature_weights[sub..sub + HIDDEN].iter()),
            );

            for (acc_val, (&add_weight, &remove_weight)) in zip {
                *acc_val += add_weight - remove_weight;
            }
        }

        let add_idx = Self::idx(add);
        let sub_idx = Self::idx(sub);
        add_sub(&mut self.white, add_idx.0, sub_idx.0);
        add_sub(&mut self.black, add_idx.1, sub_idx.1);
    }
}

/// NNUEState is simply a stack of accumulators, updated along the search tree
#[derive(Debug, Clone)]
pub struct AccumulatorStack {
    pub accs: [Accumulator; MAX_DEPTH + 1],
    pub top: usize,
}

impl AccumulatorStack {
    /// Add a new accumulator to the stack by copying the previous top
    pub fn push(&mut self) {
        self.accs[self.top + 1] = self.accs[self.top];
        self.top += 1;
    }

    /// Remove the accumulator at the top of the stack
    pub fn pop(&mut self) {
        self.top -= 1;
    }

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
        boxed.accs[0] = Accumulator::default();
        for sq in board.occupancy() {
            boxed.accs[0].update_weights::<ON>((board.piece_at(sq), sq));
        }

        boxed
    }

    /// Refresh the accumulator stack to the given board
    pub fn refresh(&mut self, board: &Board) {
        // reset the accumulator stack
        self.top = 0;
        self.accs[self.top] = Accumulator::default();

        // update the first accumulator
        for piece in Piece::ALL {
            for sq in board.piece_occupancy(piece) {
                self.accs[0].update_weights::<ON>((piece, sq));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chess::movegen::make_move::OFF;
    use chess::square::Square;

    #[test]
    fn test_nnue_stack() {
        let b = Board::default();
        let s1 = AccumulatorStack::from_board(&b);
        let mut s2 = AccumulatorStack::from_board(&b);

        s2.push();
        s2.pop();

        for i in 0..HIDDEN {
            assert_eq!(s1.accs[0].white[i], s2.accs[0].white[i]);
            assert_eq!(s1.accs[0].black[i], s2.accs[0].black[i]);
        }
        assert_eq!(s1.top, s2.top);
    }

    #[test]
    fn test_nnue_index() {
        let idx1 = Accumulator::idx((Piece::WP, Square::A8));
        let idx2 = Accumulator::idx((Piece::WP, Square::H1));
        let idx3 = Accumulator::idx((Piece::BP, Square::A1));
        let idx4 = Accumulator::idx((Piece::WK, Square::E1));

        assert_eq!(idx1, (HIDDEN * 56, HIDDEN * 384));
        assert_eq!(idx2, (HIDDEN * 7, HIDDEN * 447));
        assert_eq!(idx3, (HIDDEN * 384, HIDDEN * 56));
        assert_eq!(idx4, (HIDDEN * 324, HIDDEN * 764));
    }

    #[test]
    fn test_manual_update() {
        let b: Board = Board::default();
        let s1 = AccumulatorStack::from_board(&b);

        let old_acc = s1.accs[0];
        let mut acc1 = old_acc;
        let mut acc2 = old_acc;

        acc1.update_weights::<ON>((Piece::WP, Square::A3));
        acc1.update_weights::<OFF>((Piece::WP, Square::A3));
        acc2.add_sub_weights((Piece::WP, Square::A3), (Piece::WP, Square::A3));

        for i in 0..HIDDEN {
            assert_eq!(old_acc.white[i], acc1.white[i]);
            assert_eq!(old_acc.black[i], acc1.black[i]);
            assert_eq!(old_acc.white[i], acc2.white[i]);
            assert_eq!(old_acc.black[i], acc2.black[i]);
        }
    }

    #[test]
    fn test_incremental_updates() {
        let b1: Board = Board::default();
        let m = b1.find_move("e2e4").unwrap();
        let b2: Board = b1.make_move(m);

        let mut s1 = AccumulatorStack::from_board(&b1);
        let s2 = AccumulatorStack::from_board(&b2);

        let piece = b1.piece_at(m.get_src());
        s1.accs[0].add_sub_weights((piece, m.get_tgt()), (piece, m.get_src()));

        for i in 0..HIDDEN {
            assert_eq!(s1.accs[0].white[i], s2.accs[0].white[i]);
            assert_eq!(s1.accs[0].black[i], s2.accs[0].black[i]);
        }
    }
}
