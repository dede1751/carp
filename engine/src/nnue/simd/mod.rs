#[cfg(any(simd_avx512vnni, simd_avx512f))]
mod avx512;
#[cfg(any(simd_avx512vnni, simd_avx512f))]
pub use avx512::*;

#[cfg(any(simd_avx2, simd_avxvnni))]
mod avx2;
#[cfg(any(simd_avx2, simd_avxvnni))]
pub use avx2::*;

#[cfg(simd_neon)]
mod neon;
#[cfg(simd_neon)]
pub use neon::*;
