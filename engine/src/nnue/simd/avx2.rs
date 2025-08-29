#![allow(non_camel_case_types)]
use std::arch::x86_64::*;
use std::array;
use std::mem::size_of;

pub type VecI16 = __m256i;
pub type VecI32 = __m256i;

pub const VEC_I16_SIZE: usize = size_of::<VecI16>() / size_of::<i16>();
pub const ACC: usize = 1; // number of accumuators to use for unrolling

#[inline(always)]
pub const fn mm_shuffle(z: i32, y: i32, x: i32, w: i32) -> i32 {
    ((z) << 6) | ((y) << 4) | ((x) << 2) | (w)
}

//////////////////////////////// i16 ////////////////////////////////
#[inline(always)]
pub unsafe fn set_i16(val: i16) -> VecI16 {
    _mm256_set1_epi16(val)
}
#[inline(always)]
pub unsafe fn load_i16<const N: usize>(ptr: *const i16) -> [VecI16; N] {
    array::from_fn(|i| _mm256_load_si256(ptr.add(i * VEC_I16_SIZE).cast()))
}
#[inline(always)]
pub unsafe fn clamp_i16<const N: usize>(val: [VecI16; N], min: VecI16, max: VecI16) -> [VecI16; N] {
    array::from_fn(|i| _mm256_min_epi16(max, _mm256_max_epi16(val[i], min)))
}
#[inline(always)]
pub unsafe fn mullo_i16<const N: usize>(a: [VecI16; N], b: [VecI16; N]) -> [VecI16; N] {
    array::from_fn(|i| _mm256_mullo_epi16(a[i], b[i]))
}
#[cfg(simd_avxvnni)]
#[inline(always)]
pub unsafe fn fmadd_i16<const N: usize>(
    sum: [VecI32; N],
    a: [VecI16; N],
    b: [VecI16; N],
) -> [VecI32; N] {
    array::from_fn(|i| _mm256_dpwssd_avx_epi32(sum[i], a[i], b[i]))
}
#[cfg(simd_avx2)]
#[inline(always)]
pub unsafe fn fmadd_i16<const N: usize>(
    sum: [VecI32; N],
    a: [VecI16; N],
    b: [VecI16; N],
) -> [VecI32; N] {
    let prod_32: [VecI32; N] = array::from_fn(|i| _mm256_madd_epi16(a[i], b[i]));
    array::from_fn(|i| _mm256_add_epi32(sum[i], prod_32[i]))
}

//////////////////////////////// i32 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_i32<const N: usize>() -> [VecI32; N] {
    [_mm256_setzero_si256(); N]
}
#[inline(always)]
pub unsafe fn add_i32<const N: usize>(a: [VecI32; N], b: [VecI32; N]) -> [VecI32; N] {
    array::from_fn(|i| _mm256_add_epi32(a[i], b[i]))
}
#[inline(always)]
pub unsafe fn sum_reduce_i32<const N: usize>(val: [VecI32; N]) -> VecI32 {
    let mut sum = val[0];
    for v in val.iter().take(N).skip(1) {
        sum = _mm256_add_epi32(sum, *v);
    }
    sum
}
// See: https://stackoverflow.com/a/60109639
#[inline(always)]
pub unsafe fn hsum_i32(val: VecI32) -> i32 {
    let hi_128 = _mm256_extracti128_si256(val, 1);
    let lo_128 = _mm256_castsi256_si128(val);
    let sum_128 = _mm_add_epi32(hi_128, lo_128);

    let hi_64 = _mm_unpackhi_epi64(sum_128, sum_128);
    let sum_64 = _mm_add_epi32(hi_64, sum_128);

    let hi_32  = _mm_shuffle_epi32(sum_64, mm_shuffle(2, 3, 0, 1));
    let sum_32 = _mm_add_epi32(hi_32, sum_64);
    _mm_cvtsi128_si32(sum_32)
}