/// Search parameters are kept in this module.
/// Terminology:
///     THRESHOLD: anything takes effect AFTER this amount or BELOW this depth (always <=)
///     LOWER_LIMIT: nothing takes effect BELOW this depth (always >=)
///     BASE: flat bonus in a formula
///     MARGIN: multiplicative (usually depth) coefficient in a formula
///     FACTOR: dividing coefficient in a formula
pub use chess::params::*;
use std::mem::transmute;

pub const INFINITY: Eval = 32001; // score upper bound
pub const MATE: Eval = 32000; // mate in 0 moves
pub const LONGEST_MATE: Eval = MATE - MAX_DEPTH as Eval; // mate in x moves
pub const TB_MATE: Eval = 30000; // tb win in 0 moves
pub const LONGEST_TB_MATE: Eval = TB_MATE - MAX_DEPTH as Eval; // tb win in x moves

pub const HIST_MAX: i32 = 8192;
pub const CONT_HIST_MAX: i32 = 16384;
pub const CAP_HIST_MAX: i32 = 16384;
pub const CONT_HIST_COUNT: usize = 2;
pub const HISTORY_MAX: i32 = HIST_MAX + CONT_HIST_MAX * CONT_HIST_COUNT as i32;

pub const BIG_DELTA: Eval = 1100;

static LMR_TABLE: [[u64; 64]; 64] = unsafe { transmute(*include_bytes!("../../bins/lmr.bin")) };
pub fn lmr_reduction(depth: usize, move_count: usize) -> usize {
    LMR_TABLE[depth.min(63)][move_count.min(63)] as usize
}

macro_rules! tunable_params {
    ($($name:ident : $ty:ty = {val=$val:expr, min=$min:expr, max=$max:expr, step=$step:expr},)*) => {
        #[cfg(feature = "tune")]
        mod params {
            use super::Eval;
            use std::sync::atomic::{AtomicI32, Ordering};

            static PARAMS: P = P::new();

            pub struct P {
                $(
                    pub $name: AtomicI32,
                )*
            }

            impl P {
                pub const fn new() -> Self {
                    Self {
                        $(
                            $name: AtomicI32::new($val),
                        )*
                    }
                }

                pub fn print_options() {
                    $(
                        println!(
                            "option name {} type spin default {} min {} max {}",
                            stringify!($name),
                            Self::$name(),
                            $min,
                            $max
                        );
                    )*
                }

                pub fn print_params_ob() {
                    $(
                        println!(
                            "{}, int, {}.0, {}.0, {}.0, {}, 0.002",
                            stringify!($name),
                            Self::$name(),
                            $min,
                            $max,
                            $step
                        );
                    )*
                }

                #[inline(always)]
                pub fn set_param(name: String, val: String) {
                    // trick from akimbo since idents can't go in function names...
                    match name.as_str() {
                        $(
                            stringify!($name) => {
                                match val.parse::<$ty>() {
                                    Ok(v) => PARAMS.$name.store(
                                        i32::try_from(v).unwrap(),
                                        Ordering::Relaxed
                                    ),
                                    _ => eprintln!("Could not parse option value!"),
                                };
                            }
                        )*
                        _ => eprintln!("Unsupported option command!"),
                    }
                }

                $(
                    #[inline(always)]
                    pub fn $name() -> $ty {
                        <$ty>::try_from(PARAMS.$name.load(Ordering::Relaxed)).unwrap()
                    }
                )*
            }
        }

        #[cfg(not(feature = "tune"))]
        mod params {
            use super::Eval;

            pub struct P;

            impl P {
                pub fn print_options() {}

                pub fn print_params_ob() {
                    eprintln!("SPSA Tuning support not enabled!");
                }

                $(
                    #[inline(always)]
                    pub const fn $name() -> $ty {
                        $val
                    }
                )*
            }
        }

        pub use params::P;
    };
}

tunable_params![
    history_max_bonus: i16 = {val=1600, min=800, max=4000, step=200},
    history_factor: i16 = {val=350, min=100, max=500, step=10},
    history_offset: i16 = {val=350, min=0, max=1000, step=10},
    tt_replace_offset: usize = {val=4, min=1, max=8, step=1},
    tt_pv_scale: usize = {val=2, min=1, max=4, step=1},
    aspiration_lower_limit: usize = {val=5, min=3, max=8, step=1},
    aspiration_window: Eval = {val=25, min=1, max=50, step=3},
    lmr_threshold: usize = {val=2, min=1, max=4, step=1},
    lmr_lower_limit: usize = {val=2, min=2, max=4, step=1},
    se_lower_limit: usize = {val=8, min=6, max=12, step=1},
    rfp_threshold: usize = {val=8, min=6, max=10, step=1},
    rfp_margin: Eval = {val=80, min=20, max=160, step=5},
    rfp_improving_margin: Eval = {val=55, min=0, max=120, step=5},
    nmp_lower_limit: usize = {val=3, min=2, max=5, step=1},
    nmp_improving_margin: Eval = {val=70, min=0, max=100, step=10},
    nmp_base: usize = {val=4, min=2, max=6, step=1},
    nmp_factor: usize = {val=4, min=1, max=6, step=1},
    iir_lower_limit: usize = {val=4, min=3, max=8, step=1},
    hlp_threshold: usize = {val=2, min=1, max=4, step=1},
    hlp_base: i32 = {val=-5000, min=-8000, max=0, step=200},
    efp_threshold: usize = {val=5, min=3, max=8, step=1},
    efp_base: Eval = {val=80, min=40, max=140, step=5},
    efp_margin: Eval = {val=90, min=50, max=160, step=10},
    lmp_threshold: usize = {val=8, min=4, max=12, step=1},
    lmp_base: usize = {val=4, min=3, max=12, step=1},
    see_pruning_threshold: usize = {val=9, min=6, max=14, step=1},
    see_capture_margin: Eval = {val=-20, min=-50, max=0, step=5},
    see_quiet_margin: Eval = {val=-65, min=-120, max=0, step=10},
];
