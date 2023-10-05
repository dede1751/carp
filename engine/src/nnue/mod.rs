/// NNUE Implementation
/// Carp uses a (768->1024)x2->1 perspective net architecture, fully trained on self play data.
/// Network is initialized at compile time from the 'net.bin' file in thie bins directory.
/// A new net can be loaded by running the convert_json.py script in the scripts folder.
///
/// Huge thanks to Cosmo, author of Viridithas, for the help. The code here is heavily inspired by
/// his engine.
use std::alloc;
use std::mem;
use std::ops::{Deref, DerefMut};

use crate::search_params::*;
use chess::{
    board::Board,
    castle::rook_castling_move,
    moves::{Move, MoveType},
    piece::{Color, Piece},
    square::Square,
};

// Network Arch
const FEATURES: usize = 768;
const HIDDEN: usize = 1024;

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
static MODEL: NNUEParams = unsafe { mem::transmute(*include_bytes!("../../../bins/net.bin")) };

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
#[derive(Clone, Debug)]
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

// used for turning on/off features
const ON: i16 = 1;
const OFF: i16 = -1;

impl Accumulator {
    /// Updates weights for a single feature, either turning them on or off
    fn update<const ON: i16>(&mut self, idx: (usize, usize)) {
        fn side_update<const ON: i16>(acc: &mut SideAccumulator, idx: usize) {
            let zip = acc
                .iter_mut()
                .zip(&MODEL.feature_weights[idx..idx + HIDDEN]);

            for (acc_val, &weight) in zip {
                *acc_val += weight * ON;
            }
        }

        side_update::<ON>(&mut self.white, idx.0);
        side_update::<ON>(&mut self.black, idx.1);
    }

    /// Adds in features for the destination and removes the features of the source
    fn swap(&mut self, from: (usize, usize), to: (usize, usize)) {
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
    stack: [Accumulator; MAX_DEPTH + 1],
    top: usize,
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
        boxed.stack[0] = Accumulator::default();
        for sq in board.occupancy() {
            boxed.stack[0].update::<ON>(nnue_index(board.piece_at(sq), sq));
        }

        boxed
    }

    /// Refresh the accumulator stack to the given board
    pub fn refresh(&mut self, board: &Board) {
        // Reset the accumulator stack
        self.top = 0;
        self.stack[0] = Accumulator::default();

        // Update the first accumulator
        for piece in Piece::ALL {
            for sq in board.piece_occupancy(piece) {
                self.stack[0].update::<ON>(nnue_index(piece, sq));
            }
        }
    }

    /// Add a new accumulator to the stack by copying the previous top
    pub fn push(&mut self) {
        self.stack[self.top + 1] = self.stack[self.top].clone();
        self.top += 1;
    }

    /// Pop the top off the accumulator stack
    pub fn pop(&mut self) {
        self.top -= 1;
    }

    /// Efficiently update accumulator with the given move.
    pub fn update(&mut self, m: Move, board: &Board) {
        let (src, tgt, mt) = (m.get_src(), m.get_tgt(), m.get_type());
        let piece = board.piece_at(m.get_src());

        // Most moves only change from-to. For promotions, To is the promotion piece
        let to = if mt.is_promotion() {
            nnue_index(mt.get_promotion(board.side), tgt)
        } else {
            nnue_index(piece, tgt)
        };
        self.stack[self.top].swap(nnue_index(piece, src), to);

        // Special moves can change more than just from-to
        match mt {
            MoveType::EnPassant => {
                let ep_tgt = tgt.forward(!board.side);
                self.stack[self.top].update::<OFF>(nnue_index((!board.side).pawn(), ep_tgt));
            }
            MoveType::Castle => {
                let (rook_src, rook_tgt) = rook_castling_move(tgt);

                self.stack[self.top].swap(
                    nnue_index(board.side.rook(), rook_src),
                    nnue_index(board.side.rook(), rook_tgt)
                );
            }
            mt if mt.is_capture() => {
                let captured = board.piece_at(tgt);
                self.stack[self.top].update::<OFF>(nnue_index(captured, tgt));
            }
            _ => {}
        }
    }


    /// Evaluate the nn from the current accumulator
    /// Concatenates the accumulators based on the side to move, computes the activation function
    /// with Squared CReLu and multiplies activation by weight. The result is the sum of all these
    /// with the bias.
    /// Since we are squaring activations, we need an extra quantization pass with QA.
    pub fn evaluate(&self, side: Color) -> Eval {
        let acc = &self.stack[self.top];

        let (us, them) = match side {
            Color::White => (acc.white.iter(), acc.black.iter()),
            Color::Black => (acc.black.iter(), acc.white.iter()),
        };

        let mut out = 0;
        for (&value, &weight) in us.zip(&MODEL.output_weights[..HIDDEN]) {
            out += squared_crelu(value) * (weight as i32);
        }
        for (&value, &weight) in them.zip(&MODEL.output_weights[HIDDEN..]) {
            out += squared_crelu(value) * (weight as i32);
        }

        ((out / QA + MODEL.output_bias as i32) * SCALE / QAB) as Eval
    }
}

/// Returns white and black feature weight index for given feature
const fn nnue_index(piece: Piece, sq: Square) -> (usize, usize) {
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
            assert_eq!(
                s1.stack[0].white[i],
                s2.stack[0].white[i]
            );
            assert_eq!(
                s1.stack[0].black[i],
                s2.stack[0].black[i]
            );
        }
        assert_eq!(s1.top, s2.top);
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
        let s1 = NNUEState::from_board(&b);

        let mut old_acc = s1.stack[0].clone();
        old_acc.update::<ON>(nnue_index(Piece::WP, Square::A3));
        old_acc.update::<OFF>(nnue_index(Piece::WP, Square::A3));

        for i in 0..HIDDEN {
            assert_eq!(old_acc.white[i], s1.stack[0].white[i]);
            assert_eq!(old_acc.black[i], s1.stack[0].black[i]);
        }
    }

    #[test]
    fn test_incremental_updates() {
        let b1: Board = Board::default();
        let m = b1.find_move("e2e4").unwrap();
        let b2: Board = b1.make_move(m);

        let mut s1 = NNUEState::from_board(&b1);
        let s2 = NNUEState::from_board(&b2);

        s1.update(m, &b1);

        for i in 0..HIDDEN {
            assert_eq!(
                s1.stack[0].white[i],
                s2.stack[0].white[i]
            );
            assert_eq!(
                s1.stack[0].black[i],
                s2.stack[0].black[i]
            );
        }
    }
}
