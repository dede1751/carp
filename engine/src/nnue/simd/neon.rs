#![allow(non_camel_case_types)]
use std::arch::aarch64::*;
use std::array;
use std::mem::size_of;

pub type VecU8 = uint8x16_t;
pub type VecU16 = uint16x8_t;
pub type VecI8 = int8x16_t;
pub type VecI16 = int16x8_t;
pub type VecI32 = int32x4_t;

pub const VEC_U8_SIZE: usize = size_of::<VecU8>() / size_of::<u8>();
pub const VEC_U16_SIZE: usize = size_of::<VecU16>() / size_of::<u16>();
pub const VEC_I8_SIZE: usize = size_of::<VecI8>() / size_of::<i8>();
pub const VEC_I16_SIZE: usize = size_of::<VecI16>() / size_of::<i16>();
pub const VEC_I32_SIZE: usize = size_of::<VecI32>() / size_of::<i32>();

pub const ACC: usize = 8;

//////////////////////////////// i16 ////////////////////////////////
#[inline(always)]
pub unsafe fn set_i16(val: i16) -> VecI16 {
    vdupq_n_s16(val)
}
#[inline(always)]
pub unsafe fn load_i16<const N: usize>(ptr: *const i16) -> [VecI16; N] {
    array::from_fn(|i| vld1q_s16(ptr.add(i * VEC_I16_SIZE)))
}
#[inline(always)]
pub unsafe fn clamp_i16<const N: usize>(val: [VecI16; N], min: VecI16, max: VecI16) -> [VecI16; N] {
    array::from_fn(|i| vminq_s16(max, vmaxq_s16(val[i], min)))
}
#[inline(always)]
pub unsafe fn mullo_i16<const N: usize>(a: [VecI16; N], b: [VecI16; N]) -> [VecI16; N] {
    array::from_fn(|i| vmulq_s16(a[i], b[i]))
}
#[inline(always)]
pub unsafe fn fmadd_i16<const N: usize>(
    sum: [VecI32; N],
    a: [VecI16; N],
    b: [VecI16; N],
) -> [VecI32; N] {
    let sum_lo: [VecI32; N] =
        array::from_fn(|i| vmlal_s16(sum[i], vget_low_s16(a[i]), vget_low_s16(b[i])));
    array::from_fn(|i| vmlal_high_s16(sum_lo[i], a[i], b[i]))
}

//////////////////////////////// i32 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_i32<const N: usize>() -> [VecI32; N] {
    [vdupq_n_s32(0); N]
}
#[inline(always)]
pub unsafe fn add_i32<const N: usize>(a: [VecI32; N], b: [VecI32; N]) -> [VecI32; N] {
    array::from_fn(|i| vaddq_s32(a[i], b[i]))
}
#[inline(always)]
pub unsafe fn sum_reduce_i32<const N: usize>(val: [VecI32; N]) -> i32 {
    let mut sum = val[0];
    for i in 1..N {
        sum = vaddq_s32(sum, val[i]);
    }
    vaddvq_s32(sum)
}
