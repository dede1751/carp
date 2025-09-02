#![allow(non_camel_case_types)]
use std::arch::x86_64::*;
use std::mem::size_of;

pub type VecI16 = __m512i;
pub type VecI32 = __m512i;

pub const VEC_I16_SIZE: usize = size_of::<VecI16>() / size_of::<i16>();
pub const UNROLL: usize = VEC_I16_SIZE;

//////////////////////////////// i16 ////////////////////////////////
#[inline(always)]
pub unsafe fn set_i16(val: i16) -> VecI16 {
    _mm512_set1_epi16(val)
}
#[inline(always)]
pub unsafe fn load_i16(ptr: *const i16) -> VecI16 {
    _mm512_load_si512(ptr.cast())
}
#[inline(always)]
pub unsafe fn clamp_i16(val: VecI16, min: VecI16, max: VecI16) -> VecI16 {
    _mm512_min_epi16(max, _mm512_max_epi16(val, min))
}
#[inline(always)]
pub unsafe fn mullo_i16(a: VecI16, b: VecI16) -> VecI16 {
    _mm512_mullo_epi16(a, b)
}
#[cfg(simd_avx512vnni)]
#[inline(always)]
pub unsafe fn fmadd_i16(sum: VecI32, a: VecI16, b: VecI16) -> VecI32 {
    _mm512_dpwssd_epi32(sum, a, b)
}
#[cfg(simd_avx512f)]
#[inline(always)]
pub unsafe fn fmadd_i16(sum: VecI32, a: VecI16, b: VecI16) -> VecI32 {
    _mm512_add_epi32(sum, _mm512_madd_epi16(a, b))
}

//////////////////////////////// i32 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_i32() -> VecI32 {
    _mm512_setzero_epi32()
}
#[inline(always)]
pub unsafe fn add_i32(a: VecI32, b: VecI32) -> VecI32 {
    _mm512_add_epi32(a, b)
}
#[inline(always)]
pub unsafe fn hsum_i32(val: VecI32) -> i32 {
    _mm512_reduce_add_epi32(val)
}
