use super::accumulator::AccumulatorStack;
use super::network::{CR_MAX, CR_MIN, HIDDEN, MODEL, QA, QAB, SCALE};
use crate::search_params::Eval;
use chess::piece::Color;

#[cfg(simd_none)]
mod scalar_eval {
    use super::*;

    /// Squared Clipped ReLu activation function using the lizard trick
    #[inline(always)]
    fn lizard_screlu(value: i16, weight: i16) -> i32 {
        let v = value.clamp(CR_MIN, CR_MAX);
        let vw = v * weight;
        (v as i32) * (vw as i32)
    }

    impl AccumulatorStack {
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

    impl AccumulatorStack {
        pub fn evaluate(&self, side: Color) -> Eval {
            let acc = &self.accs[self.top];
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
