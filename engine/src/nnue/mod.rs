/// NNUE Implementation
/// Carp uses a (768->1024)x2->1 perspective net architecture, fully trained on self play data.
/// Network is initialized at compile time from the 'net.bin' file in thie bins directory.
/// A new net can be loaded by running the convert_json.py script in the scripts folder.
///
/// Huge thanks to Cosmo, author of Viridithas, for the help. The code here is heavily inspired by
/// his engine.
mod accumulator;
mod inference;
mod network;
mod simd;

pub use accumulator::{AccumulatorStack, NNUEState};

use std::ops::{Deref, DerefMut};

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
