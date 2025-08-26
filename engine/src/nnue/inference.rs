use crate::nnue::accumulator::NNUEState;

use super::accumulator::{Accumulator, SideAccumulator};
use super::network::{CR_MAX, CR_MIN, HIDDEN, MODEL, QA, QAB, SCALE};
use crate::search_params::Eval;
use chess::piece::Color;

#[cfg(simd_none)]
mod scalar_eval {
    use super::*;

    impl Accumulator {
        /// Updates weights for a single feature, either turning them on or off
        pub fn update_weights<const ON: bool>(&mut self, idx: (usize, usize)) {
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
        pub fn add_sub_weights(&mut self, from: (usize, usize), to: (usize, usize)) {
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

    /// Squared Clipped ReLu activation function
    fn squared_crelu(value: i16) -> i32 {
        let v = value.clamp(CR_MIN, CR_MAX) as i32;
        v * v
    }
    
    impl NNUEState {
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
}

#[cfg(not(simd_none))]
mod simd_eval {
    use super::*;
    //use crate::nnue::simd::*;

    impl Accumulator {
        /// Updates weights for a single feature, either turning them on or off
        pub fn update_weights<const ON: bool>(&mut self, idx: (usize, usize)) {
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
        pub fn add_sub_weights(&mut self, from: (usize, usize), to: (usize, usize)) {
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
    
    /// Squared Clipped ReLu activation function
    fn squared_crelu(value: i16) -> i32 {
        let v = value.clamp(CR_MIN, CR_MAX) as i32;
        v * v
    }
    
    impl NNUEState {
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
}
