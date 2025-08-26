#![allow(
    clippy::all,
    clippy::nursery,
    clippy::pedantic,
    dead_code,
    unused_imports
)]
use std::mem::size_of;

mod neon;

#[cfg(simd_avx512)]
pub use avx512::*;

#[cfg(simd_avx2)]
pub use avx2::*;

#[cfg(simd_sse2)]
pub use sse2::*;

#[cfg(simd_neon)]
pub use neon::*;

