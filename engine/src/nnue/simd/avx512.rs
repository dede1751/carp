#![allow(non_camel_case_types)]
use std::arch::avx512f::*;
use std::array;
use std::mem::size_of;
use super::mm_shuffle;

pub type VecI16 = __m512i;
pub type VecI32 = __m512i;

pub const VEC_I16_SIZE: usize = size_of::<VecI16>() / size_of::<i16>();
pub const ACC: usize = 2; // number of accumuators to use for unrolling

//////////////////////////////// i16 ////////////////////////////////
#[inline(always)]
pub unsafe fn set_i16(val: i16) -> VecI16 {
    _mm512_set1_epi16(val)
}
#[inline(always)]
pub unsafe fn load_i16<const N: usize>(ptr: *const i16) -> [VecI16; N] {
    array::from_fn(|i| _mm512_load_si512(ptr.add(i * VEC_I16_SIZE)))
}
#[inline(always)]
pub unsafe fn clamp_i16<const N: usize>(val: [VecI16; N], min: VecI16, max: VecI16) -> [VecI16; N] {
    array::from_fn(|i| _mm512_min_epi16(max, _mm512_max_epi16(val[i], min)))
}
#[inline(always)]
pub unsafe fn mullo_i16<const N: usize>(a: [VecI16; N], b: [VecI16; N]) -> [VecI16; N] {
    array::from_fn(|i| _mm512_mullo_epi16(a[i], b[i]))
}
#[cfg(simd_avx512vnni)]
#[inline(always)]
pub unsafe fn fmadd_i16<const N: usize>(
    sum: [VecI32; N],
    a: [VecI16; N],
    b: [VecI16; N],
) -> [VecI32; N] {
    array::from_fn(|i| _mm512_dpwssd_epi32(sum[i], a[i], b[i]))
}
#[cfg(simd_avx512f)]
#[inline(always)]
pub unsafe fn fmadd_i16<const N: usize>(
    sum: [VecI32; N],
    a: [VecI16; N],
    b: [VecI16; N],
) -> [VecI32; N] {
    let prod_32 = array::from_fn(|i| _mm512_madd_epi16(a[i], b[i]));
    array::from_fn(|i| _mm512_add_epi32(sum[i], prod_32[i]))
}

//////////////////////////////// i32 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_i32<const N: usize>() -> [VecI32; N] {
    [_mm512_setzero_epi32(); N]
}
#[inline(always)]
pub unsafe fn add_i32<const N: usize>(a: [VecI32; N], b: [VecI32; N]) -> [VecI32; N] {
    array::from_fn(|i| _mm512_add_epi32(a[i], b[i]))
}
#[inline(always)]
pub unsafe fn sum_reduce_i32<const N: usize>(val: [VecI32; N]) -> VecI32 {
    let mut sum = val[0];
    for v in val.iter().take(N).skip(1) {
        sum = _mm512_add_epi32(sum, *v);
    }
    sum
}
// See: https://stackoverflow.com/a/60109639
#[inline(always)]
pub unsafe fn hsum_i32(val: VecI32) -> i32 {
    _mm512_reduce_add_epi32(val)
}