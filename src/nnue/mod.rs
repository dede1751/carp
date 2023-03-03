/// NNUE Implementation
/// Carp uses a 768->256->1 perspective net architecture, fully trained on self play data.
/// Network is initialized at compile time from the binary files in the net folder.
/// A new net can be loaded by running the convert_json.py script in the scripts folder.
///
/// Huge thanks to Cosmo, author of Viridithas, for the help. The code here is heavily inspired by
/// his engine.

use std::mem;
use std::alloc;

use crate::board::*;
use crate::piece::*;
use crate::search_params::*;
use crate::square::*;

// network arch
const FEATURES: usize = 768;
const HIDDEN: usize = 256;

// clipped relu bounds
const CR_MIN: i16 = 0;
const CR_MAX: i16 = 255;

// quantization factor
const QAB: i32 = 255 * 64;

// eval scale factor
const SCALE: i32 = 400;

type SideAccumulator = [i16; HIDDEN];

/// Accumulators contain the efficiently updated hidden layer values
/// Each accumulator is perspective, hence both contains the white and black pov
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug)]
struct Accumulator {
    white: SideAccumulator,
    black: SideAccumulator,
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
    /// Updates weights for a single feature, either turning them on or off
    fn update_weights<const ON: bool>(&mut self, idx: (usize, usize)) {
        fn update<const ON: bool>(acc: &mut SideAccumulator, idx: usize) {
            let zip = acc
                .iter_mut()
                .zip(&MODEL.feature_weights[idx..idx + HIDDEN]);

            for (acc_val, &weight) in zip {
                if ON {
                    *acc_val += weight;
                } else {
                    *acc_val -= weight;
                }
            }
        }

        update::<ON>(&mut self.white, idx.0);
        update::<ON>(&mut self.black, idx.1);
    }

    /// Update accumulator for a quiet move.
    /// Adds in features for the destination and removes the features of the source
    fn add_sub_weights(&mut self, from: (usize, usize), to: (usize, usize)) {
        fn add_sub(acc: &mut SideAccumulator, from: usize, to: usize) {
            let zip = acc.iter_mut().zip(
                MODEL.feature_weights[from..from + HIDDEN]
                    .iter()
                    .zip(&MODEL.feature_weights[to..to + HIDDEN]),
            );

            for (acc_val, (&remove_weight, &add_weight)) in zip {
                let new_val = add_weight - remove_weight;

                *acc_val += new_val
            }
        }

        add_sub(&mut self.white, from.0, to.0);
        add_sub(&mut self.black, from.1, to.1);
    }
}

/// NNUE model is initialized from binary values
/// Taken from Viri
static MODEL: NNUEParams = NNUEParams {
    feature_weights: unsafe { mem::transmute(*include_bytes!("net/feature_weights.bin")) },
    feature_bias: unsafe { mem::transmute(*include_bytes!("net/feature_bias.bin")) },
    output_weights: unsafe { mem::transmute(*include_bytes!("net/output_weights.bin")) },
    output_bias: unsafe { mem::transmute(*include_bytes!("net/output_bias.bin")) },
};

struct NNUEParams {
    feature_weights: [i16; FEATURES * HIDDEN],
    feature_bias: [i16; HIDDEN],
    output_weights: [i16; HIDDEN * 2],
    output_bias: i16,
}

/// NNUEState is simply a stack of accumulators, updated along the search tree
#[derive(Debug, Clone)]
pub struct NNUEState {
    accumulator_stack: [Accumulator; MAX_DEPTH],
    current_acc: usize,
}

// used for turning on/off features
pub const ON: bool = true;
pub const OFF: bool = false;

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
        for piece in ALL_PIECES {
            for sq in board.pieces[piece as usize] {
                boxed.manual_update::<ON>(piece, sq);
            }
        }

        boxed
    }

    /// Add a new accumulator to the stack by copying the previous top
    pub fn push(&mut self) {
        self.accumulator_stack[self.current_acc + 1] = self.accumulator_stack[self.current_acc];
        self.current_acc += 1;
    }

    /// Pop the top off the accumulator stack
    pub fn pop(&mut self) {
        self.current_acc -= 1;
    }

    /// Manually turn on or off the single given feature
    pub fn manual_update<const ON: bool>(&mut self, piece: Piece, sq: Square) {
        let idx = nnue_index(piece, sq);

        self.accumulator_stack[self.current_acc].update_weights::<ON>(idx);
    }

    /// Efficiently update accumulator for a quiet move (that is, only changes from/to features)
    pub fn move_update(&mut self, piece: Piece, from: Square, to: Square) {
        let from_idx = nnue_index(piece, from);
        let to_idx = nnue_index(piece, to);

        self.accumulator_stack[self.current_acc].add_sub_weights(from_idx, to_idx);
    }

    /// Evaluate the nn from the current accumulator
    /// Concatenates the accumulators based on the side to move, computes the activation function
    /// with Clipped ReLu and multiplies activation by weight. The result is the sum of all these
    /// with the bias
    pub fn evaluate(&self, side: Color) -> Eval {
        let acc = &self.accumulator_stack[self.current_acc];

        let (us, them) = match side {
            Color::White => (acc.white.iter(), acc.black.iter()),
            Color::Black => (acc.black.iter(), acc.white.iter()),
        };

        let mut out = MODEL.output_bias as i32;
        for (&value, &weight) in us.zip(&MODEL.output_weights[..HIDDEN]) {
            out += (value.clamp(CR_MIN, CR_MAX) as i32) * (weight as i32);
        }
        for (&value, &weight) in them.zip(&MODEL.output_weights[HIDDEN..]) {
            out += (value.clamp(CR_MIN, CR_MAX) as i32) * (weight as i32);
        }

        (out * SCALE / QAB) as Eval
    }
}

/// Returns white and black feature weight index for given feature
fn nnue_index(piece: Piece, sq: Square) -> (usize, usize) {
    const COLOR_STRIDE: usize = 64 * 6;
    const PIECE_STRIDE: usize = 64;
    let p = (piece as usize) / 2;
    let c = piece.color() as usize;

    let white_idx = c * COLOR_STRIDE + p * PIECE_STRIDE + sq.flipv() as usize;
    let black_idx = (1 ^ c) * COLOR_STRIDE + p * PIECE_STRIDE + sq as usize;

    (white_idx * HIDDEN, black_idx * HIDDEN)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_nnue_stack() {
        let b = Board::default();
        let s1 = NNUEState::from_board(&b);
        let mut s2 = NNUEState::from_board(&b);

        s2.push();
        s2.pop();

        assert_eq!(s1.accumulator_stack[0], s2.accumulator_stack[0]);
        assert_eq!(s1.current_acc, s2.current_acc);
    }

    #[test]
    fn test_nnue_index() {
        let idx1 = nnue_index(Piece::WP, Square::A8);
        let idx2 = nnue_index(Piece::WP, Square::H1);
        let idx3 = nnue_index(Piece::BP, Square::A1);
        let idx4 = nnue_index(Piece::WK, Square::E1);

        assert_eq!(idx1, (14336, 98304));
        assert_eq!(idx2, (1792, 114432));
        assert_eq!(idx3, (98304, 14336));
        assert_eq!(idx4, (82944, 195584));
    }

    #[test]
    fn test_manual_update() {
        let b: Board = Board::default();
        let mut s1 = NNUEState::from_board(&b);

        let old_acc = s1.accumulator_stack[0];

        s1.manual_update::<ON>(Piece::WP, Square::A3);
        s1.manual_update::<OFF>(Piece::WP, Square::A3);

        assert_eq!(old_acc, s1.accumulator_stack[0]);
    }

    #[test]
    fn test_incremental_updates() {
        let b1: Board = Board::default();
        let m = b1.find_move("e2e4").unwrap();
        let b2: Board = b1.make_move(m);

        let mut s1 = NNUEState::from_board(&b1);
        let s2 = NNUEState::from_board(&b2);

        s1.move_update(m.get_piece(), m.get_src(), m.get_tgt());

        assert_eq!(s1.accumulator_stack[0], s2.accumulator_stack[0]);
    }
}
