#![allow(non_camel_case_types)]
use std::arch::aarch64::*;

pub const VEC_SIZE_BYTES: usize = 16;

wrap_simd_register!(uint8x16_t, u8, VecU8);
wrap_simd_register!(uint16x8_t, u16, VecU16);
wrap_simd_register!(int8x16_t, i8, VecI8);
wrap_simd_register!(int16x8_t, i16, VecI16);
wrap_simd_register!(int32x4_t, i32, VecI32);

// NEON-specific reinterpret casts 
pub unsafe fn reinterpret_i32s_as_i8s(vec: VecI32) -> VecI8 {
    VecI8::from_raw(vreinterpretq_s8_s32(vec.inner()))
}

pub unsafe fn reinterpret_i8s_as_i32s(vec: VecI8) -> VecI32 {
    VecI32::from_raw(vreinterpretq_s32_s8(vec.inner()))
}

// This could ALL become a macro, if only we could put idents in
// function names. Sadly, I do not want to depend on `paste`

//////////////////////////////// u8 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_u8() -> VecU8 {
    VecU8::from_raw(vdupq_n_u8(0))
}
#[inline(always)]
pub unsafe fn splat_u8(val: u8) -> VecU8 {
    VecU8::from_raw(vdupq_n_u8(val))
}
#[inline(always)]
pub unsafe fn load_u8(ptr: *const u8) -> VecU8 {
    VecU8::from_raw(vld1q_u8(ptr))
}
#[inline(always)]
pub unsafe fn store_u8(ptr: *mut u8, vec: VecU8) {
    vst1q_u8(ptr, vec.inner());
}
#[inline(always)]
pub unsafe fn add_u8(a: VecU8, b: VecU8) -> VecU8 {
    VecU8::from_raw(vaddq_u8(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn sub_u8(a: VecU8, b: VecU8) -> VecU8 {
    VecU8::from_raw(vsubq_u8(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn min_u8(a: VecU8, b: VecU8) -> VecU8 {
    VecU8::from_raw(vminq_u8(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn max_u8(a: VecU8, b: VecU8) -> VecU8 {
    VecU8::from_raw(vmaxq_u8(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn clamp_u8(val: VecU8, min: VecU8, max: VecU8) -> VecU8 {
    min_u8(max, max_u8(val, min))
}

//////////////////////////////// u16 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_u16() -> VecU16 {
    VecU16::from_raw(vdupq_n_u16(0))
}
#[inline(always)]
pub unsafe fn splat_u16(val: u16) -> VecU16 {
    VecU16::from_raw(vdupq_n_u16(val))
}
#[inline(always)]
pub unsafe fn load_u16(ptr: *const u16) -> VecU16 {
    VecU16::from_raw(vld1q_u16(ptr))
}
#[inline(always)]
pub unsafe fn store_u16(ptr: *mut u16, vec: VecU16) {
    vst1q_u16(ptr, vec.inner());
}
#[inline(always)]
pub unsafe fn add_u16(a: VecU16, b: VecU16) -> VecU16 {
    VecU16::from_raw(vaddq_u16(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn sub_u16(a: VecU16, b: VecU16) -> VecU16 {
    VecU16::from_raw(vsubq_u16(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn min_u16(a: VecU16, b: VecU16) -> VecU16 {
    VecU16::from_raw(vminq_u16(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn max_u16(a: VecU16, b: VecU16) -> VecU16 {
    VecU16::from_raw(vmaxq_u16(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn clamp_u16(val: VecU16, min: VecU16, max: VecU16) -> VecU16 {
    min_u16(max, max_u16(val, min))
}

//////////////////////////////// i8 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_i8() -> VecI8 {
    VecI8::from_raw(vdupq_n_s8(0))
}
#[inline(always)]
pub unsafe fn splat_i8(val: i8) -> VecI8 {
    VecI8::from_raw(vdupq_n_s8(val))
}
#[inline(always)]
pub unsafe fn load_i8(ptr: *const i8) -> VecI8 {
    VecI8::from_raw(vld1q_s8(ptr))
}
#[inline(always)]
pub unsafe fn store_i8(ptr: *mut i8, vec: VecI8) {
    vst1q_s8(ptr, vec.inner());
}
#[inline(always)]
pub unsafe fn add_i8(a: VecI8, b: VecI8) -> VecI8 {
    VecI8::from_raw(vaddq_s8(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn sub_i8(a: VecI8, b: VecI8) -> VecI8 {
    VecI8::from_raw(vsubq_s8(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn min_i8(a: VecI8, b: VecI8) -> VecI8 {
    VecI8::from_raw(vminq_s8(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn max_i8(a: VecI8, b: VecI8) -> VecI8 {
    VecI8::from_raw(vmaxq_s8(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn clamp_i8(val: VecI8, min: VecI8, max: VecI8) -> VecI8 {
    min_i8(max, max_i8(val, min))
}

//////////////////////////////// i16 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_i16() -> VecI16 {
    VecI16::from_raw(vdupq_n_s16(0))
}
#[inline(always)]
pub unsafe fn splat_i16(val: i16) -> VecI16 {
    VecI16::from_raw(vdupq_n_s16(val))
}
#[inline(always)]
pub unsafe fn load_i16(ptr: *const i16) -> VecI16 {
    VecI16::from_raw(vld1q_s16(ptr))
}
#[inline(always)]
pub unsafe fn store_i16(ptr: *mut i16, vec: VecI16) {
    vst1q_s16(ptr, vec.inner());
}
#[inline(always)]
pub unsafe fn add_i16(a: VecI16, b: VecI16) -> VecI16 {
    VecI16::from_raw(vaddq_s16(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn sub_i16(a: VecI16, b: VecI16) -> VecI16 {
    VecI16::from_raw(vsubq_s16(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn min_i16(a: VecI16, b: VecI16) -> VecI16 {
    VecI16::from_raw(vminq_s16(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn max_i16(a: VecI16, b: VecI16) -> VecI16 {
    VecI16::from_raw(vmaxq_s16(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn clamp_i16(val: VecI16, min: VecI16, max: VecI16) -> VecI16 {
    min_i16(max, max_i16(val, min))
}

//////////////////////////////// i32 ////////////////////////////////
#[inline(always)]
pub unsafe fn zero_i32() -> VecI32 {
    VecI32::from_raw(vdupq_n_s32(0))
}
#[inline(always)]
pub unsafe fn splat_i32(val: i32) -> VecI32 {
    VecI32::from_raw(vdupq_n_s32(val))
}
#[inline(always)]
pub unsafe fn load_i32(ptr: *const i32) -> VecI32 {
    VecI32::from_raw(vld1q_s32(ptr))
}
#[inline(always)]
pub unsafe fn store_i32(ptr: *mut i32, vec: VecI32) {
    vst1q_s32(ptr, vec.inner());
}
#[inline(always)]
pub unsafe fn add_i32(a: VecI32, b: VecI32) -> VecI32 {
    VecI32::from_raw(vaddq_s32(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn sub_i32(a: VecI32, b: VecI32) -> VecI32 {
    VecI32::from_raw(vsubq_s32(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn min_i32(a: VecI32, b: VecI32) -> VecI32 {
    VecI32::from_raw(vminq_s32(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn max_i32(a: VecI32, b: VecI32) -> VecI32 {
    VecI32::from_raw(vmaxq_s32(a.inner(), b.inner()))
}
#[inline(always)]
pub unsafe fn clamp_i32(val: VecI32, min: VecI32, max: VecI32) -> VecI32 {
    min_i32(max, max_i32(val, min))
}
