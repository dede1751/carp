use super::Align64;

use std::mem::transmute;

// Network Arch
pub const FEATURES: usize = 768;
pub const HIDDEN: usize = 1024;

// Clipped ReLu bounds
pub const CR_MIN: i16 = 0;
pub const CR_MAX: i16 = 255;

// Quantization factors
pub const QA: i32 = 255;
pub const QAB: i32 = 255 * 64;

// Eval scaling factor
pub const SCALE: i32 = 400;

/// Container for all network parameters
#[repr(C)]
pub struct NNUEParams {
    pub feature_weights: Align64<[i16; FEATURES * HIDDEN]>,
    pub feature_bias: Align64<[i16; HIDDEN]>,
    pub output_weights: Align64<[i16; HIDDEN * 2]>,
    pub output_bias: i16,
}

/// NNUE model is initialized from binary values (Viridithas format)
pub static MODEL: NNUEParams = unsafe { transmute(*include_bytes!("../../../../bins/net.bin")) };
