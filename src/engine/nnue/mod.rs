/// NNUE Implementation
/// Carp uses a 768->768->1 perspective net architecture, fully trained on self play data.
/// Network is initialized at compile time from the 'net.bin' file in this directory.
/// A new net can be loaded by running the convert_json.py script in the scripts folder.
///
/// Huge thanks to Cosmo, author of Viridithas, for the help. The code here is heavily inspired by
/// his engine.
use std::alloc;
use std::mem;
use std::ops::{Deref, DerefMut};

use super::search_params::*;
use crate::chess::{board::*, piece::*, square::*};

// Network Arch
const FEATURES: usize = 768;
const HIDDEN: usize = 768;

// Clipped ReLu bounds
const CR_MIN: i16 = 0;
const CR_MAX: i16 = 255;

// Quantization factors
const QA: i32 = 255;
const QAB: i32 = 255 * 64;

// Eval scaling factor
const SCALE: i32 = 400;

/// Container for all network parameters
#[repr(C)]
struct NNUEParams {
    feature_weights: Align64<[i16; FEATURES * HIDDEN]>,
    feature_bias: Align64<[i16; HIDDEN]>,
    output_weights: Align64<[i16; HIDDEN * 2]>,
    output_bias: i16,
}

/// NNUE model is initialized from binary values (Viridithas format)
static MODEL: NNUEParams = unsafe { mem::transmute(*include_bytes!("net.bin")) };

/// Generic wrapper for types aligned to 64B for AVX512 (also a Viridithas trick)
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(C, align(64))]
struct Align64<T>(pub T);

impl<T, const SIZE: usize> Deref for Align64<[T; SIZE]> {
    type Target = [T; SIZE];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T, const SIZE: usize> DerefMut for Align64<[T; SIZE]> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

type SideAccumulator = Align64<[i16; HIDDEN]>;

/// Accumulators contain the efficiently updated hidden layer values
/// Each accumulator is perspective, hence both contains the white and black pov
#[derive(Clone, Copy, Debug)]
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
                *acc_val += add_weight - remove_weight;
            }
        }

        add_sub(&mut self.white, from.0, to.0);
        add_sub(&mut self.black, from.1, to.1);
    }
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
        for sq in board.occupancy {
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
        for piece in ALL_PIECES {
            for sq in board.piece_bb[piece as usize] {
                self.manual_update::<ON>(piece, sq);
            }
        }
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
        self.accumulator_stack[self.current_acc].update_weights::<ON>(nnue_index(piece, sq));
    }

    /// Efficiently update accumulator for a quiet move (that is, only changes from/to features)
    pub fn move_update(&mut self, piece: Piece, from: Square, to: Square) {
        let from_idx = nnue_index(piece, from);
        let to_idx = nnue_index(piece, to);

        self.accumulator_stack[self.current_acc].add_sub_weights(from_idx, to_idx);
    }

    /// Evaluate the nn from the current accumulator
    /// Concatenates the accumulators based on the side to move, computes the activation function
    /// with Squared CReLu and multiplies activation by weight. The result is the sum of all these
    /// with the bias.
    /// Since we are squaring activations, we need an extra quantization pass with QA.
    pub fn evaluate(&self, side: Color) -> Eval {
        let acc = &self.accumulator_stack[self.current_acc];

        let (us, them) = match side {
            Color::White => (acc.white.iter(), acc.black.iter()),
            Color::Black => (acc.black.iter(), acc.white.iter()),
        };

        let mut out = MODEL.output_bias as i32;
        for (&value, &weight) in us.zip(&MODEL.output_weights[..HIDDEN]) {
            out += squared_crelu(value) * (weight as i32);
        }
        for (&value, &weight) in them.zip(&MODEL.output_weights[HIDDEN..]) {
            out += squared_crelu(value) * (weight as i32);
        }

        ((out / QA) * SCALE / QAB) as Eval
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

/// Squared Clipped ReLu activation function
fn squared_crelu(value: i16) -> i32 {
    let v = value.clamp(CR_MIN, CR_MAX) as i32;

    v * v
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

        for i in 0..HIDDEN {
            assert_eq!(s1.accumulator_stack[0].white[i], s2.accumulator_stack[0].white[i]);
            assert_eq!(s1.accumulator_stack[0].black[i], s2.accumulator_stack[0].black[i]);
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
            assert_eq!(s1.accumulator_stack[0].white[i], s2.accumulator_stack[0].white[i]);
            assert_eq!(s1.accumulator_stack[0].black[i], s2.accumulator_stack[0].black[i]);
        }
    }
}
