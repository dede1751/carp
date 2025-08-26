#![allow(clippy::all, clippy::nursery, clippy::pedantic, dead_code, unused_imports)]

/// Yoinked from Viridithas: https://github.com/cosmobobak/viridithas/blob/master/src/nnue/simd.rs
/// Given a regular type and a SIMD register type, and the new type name, create a new type that wraps the register type.
#[allow(unused_macros)]
macro_rules! wrap_simd_register {
    ($register_type:ty, $held_type:ty, $new_type:ident) => {
        #[repr(transparent)]
        #[derive(Debug, Clone, Copy)]
        pub struct $new_type($register_type);
        impl $new_type {
            #[inline(always)]
            pub const fn from_raw(value: $register_type) -> Self {
                Self(value)
            }
            #[inline(always)]
            pub const fn inner(self) -> $register_type {
                self.0
            }
            #[inline(always)]
            pub const fn len() -> usize {
                VEC_SIZE_BYTES / std::mem::size_of::<$held_type>()
            }
        }
    };
}

mod neon;

#[cfg(simd_avx512)]
pub use avx512::*;

#[cfg(simd_avx2)]
pub use avx2::*;

#[cfg(simd_sse2)]
pub use sse2::*;

#[cfg(simd_neon)]
pub use neon::*;


// All non-neon reinterpret casts can just rely on the same underlying register type for all Vecs
#[cfg(all(not(simd_none), not(simd_neon)))]
#[inline(always)]
pub fn reinterpret_i32s_as_i8s(vec: VecI32) -> VecI8 {
    VecI8::from_raw(vec.inner())
}

#[cfg(all(not(simd_none), not(simd_neon)))]
#[inline(always)]
pub fn reinterpret_i8s_as_i32s(vec: VecI8) -> VecI32 {
    VecI32::from_raw(vec.inner())
}
