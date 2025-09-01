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

    /// Squared Clipped ReLu activation function using the lizard trick
    #[inline(always)]
    fn lizard_screlu(value: i16, weight: i16) -> i32 {
        let v = value.clamp(CR_MIN, CR_MAX);
        let vw = v * weight;
        (v as i32) * (vw as i32)
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
                out += lizard_screlu(value, weight);
            }
            for (&value, &weight) in them.zip(&MODEL.output_weights[HIDDEN..]) {
                out += lizard_screlu(value, weight);
            }

            ((out / QA + MODEL.output_bias as i32) * SCALE / QAB) as Eval
        }
    }
}

#[cfg(not(simd_none))]
mod simd_eval {
    use super::*;
    use crate::nnue::simd::{self, UNROLL};

    impl Accumulator {
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

    impl NNUEState {
        pub fn evaluate(&self, side: Color) -> Eval {
            let acc = &self.accumulator_stack[self.current_acc];
            let (us, them) = match side {
                Color::White => (&acc.white, &acc.black),
                Color::Black => (&acc.black, &acc.white),
            };

            let out = unsafe {
                let cr_min = simd::set_i16(CR_MIN);
                let cr_max = simd::set_i16(CR_MAX);
                let mut sum1 = simd::zero_i32();
                let mut sum2 = simd::zero_i32();

                let x1_ptr = us.as_ptr();
                let x2_ptr = them.as_ptr();
                let w1_ptr = MODEL.output_weights.as_ptr();
                let w2_ptr = w1_ptr.add(HIDDEN);

                for i in (0..HIDDEN).step_by(UNROLL) {
                    let x1 = simd::load_i16(x1_ptr.add(i));
                    let x2 = simd::load_i16(x2_ptr.add(i));
                    let w1 = simd::load_i16(w1_ptr.add(i));
                    let w2 = simd::load_i16(w2_ptr.add(i));

                    let v1 = simd::clamp_i16(x1, cr_min, cr_max);
                    let v2 = simd::clamp_i16(x2, cr_min, cr_max);

                    let vw1 = simd::mullo_i16(v1, w1);
                    let vw2 = simd::mullo_i16(v2, w2);

                    sum1 = simd::fmadd_i16(sum1, v1, vw1);
                    sum2 = simd::fmadd_i16(sum2, v2, vw2);
                }

                simd::hsum_i32(simd::add_i32(sum1, sum2))
            };

            ((out / QA + MODEL.output_bias as i32) * SCALE / QAB) as Eval
        }
    }
}
