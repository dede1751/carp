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

#[allow(dead_code)]
#[inline(always)]
pub const fn mm_shuffle(z: i32, y: i32, x: i32, w: i32) -> i32 {
    ((z) << 6) | ((y) << 4) | ((x) << 2) | (w)
}
