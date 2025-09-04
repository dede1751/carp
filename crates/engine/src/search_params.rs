/// Search parameters are kept in this module.
/// Terminology:
///     THRESHOLD: anything takes effect AFTER this amount or BELOW this depth (always <=)
///     LOWER_LIMIT: nothing takes effect BELOW this depth (always >=)
///     BASE: flat bonus in a formula
///     MARGIN: multiplicative (usually depth) coefficient in a formula
///     FACTOR: dividing coefficient in a formula
pub type Eval = i32;
pub const MAX_DEPTH: usize = 127;

pub const INFINITY: Eval = 32001; // score upper bound
pub const MATE: Eval = 32000; // mate in 0 moves
pub const LONGEST_MATE: Eval = MATE - MAX_DEPTH as Eval; // mate in x moves
pub const TB_MATE: Eval = 30000; // tb win in 0 moves
pub const LONGEST_TB_MATE: Eval = TB_MATE - MAX_DEPTH as Eval; // tb win in x moves

// History table sizing. We avoid tuning this.
pub const HIST_MAX: i32 = 8192;
pub const CONT_HIST_MAX: i32 = 16384;
pub const CAP_HIST_MAX: i32 = 16384;
pub const CONT_HIST_COUNT: usize = 2;
pub const HISTORY_MAX: i32 = HIST_MAX + CONT_HIST_MAX * CONT_HIST_COUNT as i32;

#[cfg(not(feature = "tune"))]
mod lookups {
    use super::*;
    use chess::piece::PieceType;

    #[rustfmt::skip]
    const PIECE_VALUES: [Eval; PieceType::COUNT] = [
        P::pawn(), P::knight(), P::bishop(), P::rook(), P::queen(), 0,
    ];

    static LMR_TABLE: [[u64; 64]; 64] =
        unsafe { std::mem::transmute(*include_bytes!("../../../bins/lmr.bin")) };

    #[inline(always)]
    pub const fn piece_value(piece: PieceType) -> Eval {
        PIECE_VALUES[piece.index()]
    }

    #[inline(always)]
    pub fn lmr_reduction(depth: usize, move_count: usize) -> usize {
        LMR_TABLE[depth.min(63)][move_count.min(63)] as usize
    }
}

#[cfg(feature = "tune")]
mod lookups {
    use super::*;
    use chess::piece::PieceType;

    #[inline(always)]
    pub fn piece_value(piece: PieceType) -> Eval {
        match piece.index() {
            PieceType::Pawn => P::pawn(),
            PieceType::Knight => P::knight(),
            PieceType::Bishop => P::bishop(),
            PieceType::Rook => P::rook(),
            PieceType::Queen => P::queen(),
            PieceType::King => 0,
            _ => unreachable!(),
        }
    }

    #[inline(always)]
    pub fn lmr_reduction(depth: usize, move_count: usize) -> usize {
        let d = depth.min(63) as f32;
        let m = move_count.min(63) as f32;
        (P::lmr_base() + d.ln() * m.ln() / P::lmr_factor()) as usize
    }
}
pub use lookups::*;

macro_rules! tunable_params {
    ($($name:ident : $ty:ty = {val=$val:expr, min=$min:expr, max=$max:expr, step=$step:expr},)*) => {
        #[cfg(feature = "tune")]
        mod params {
            use super::Eval;
            use std::sync::atomic::{AtomicI16, AtomicI32, AtomicUsize, Ordering};

            trait ParamStore: Sized {
                type Raw: Copy;
                type Atomic;
                const TYPE_STRING: &'static str;
                fn to_store(v: Self) -> Self::Raw;
                fn from_store(x: Self::Raw) -> Self;
            }

            impl ParamStore for i32 {
                type Raw = i32;
                type Atomic = AtomicI32;
                const TYPE_STRING: &'static str = "int";
                #[inline] fn to_store(v: Self) -> Self::Raw { v }
                #[inline] fn from_store(x: Self::Raw) -> Self { x }
            }

            impl ParamStore for i16 {
                type Raw = i16;
                type Atomic = AtomicI16;
                const TYPE_STRING: &'static str = "int";
                #[inline] fn to_store(v: Self) -> Self::Raw { v }
                #[inline] fn from_store(x: Self::Raw) -> Self { x }
            }

            impl ParamStore for usize {
                type Raw = usize;
                type Atomic = AtomicUsize;
                const TYPE_STRING: &'static str = "int";
                #[inline] fn to_store(v: Self) -> Self::Raw { v }
                #[inline] fn from_store(x: Self::Raw) -> Self { x }
            }

            impl ParamStore for f32 {
                type Raw = i32;
                type Atomic = AtomicI32;
                const TYPE_STRING: &'static str = "float";
                #[inline] fn to_store(v: Self) -> Self::Raw { v.to_bits() as i32 }
                #[inline] fn from_store(x: Self::Raw) -> Self { f32::from_bits(x as u32) }
            }

            static PARAMS: P = P::new();

            pub struct P {
                $(
                    $name: <$ty as ParamStore>::Atomic,
                )*
            }

            impl P {
                pub const fn new() -> Self {
                    Self {$(
                        $name: <$ty as ParamStore>::Atomic::new(0),
                    )*}
                }

                pub fn init() {
                    $(
                        P::set_param(stringify!($name).to_string(), $val.to_string());
                    )*
                }

                pub fn print_options() {
                    $(
                        println!(
                            "option name {} type string default {:?}",
                            stringify!($name),
                            Self::$name() as f32,
                        );
                    )*
                }

                pub fn print_params_ob() {
                    $(
                        println!(
                            "{}, {}, {:?}, {:?}, {:?}, {:?}, 0.002", // :? also prints .0 for ints!
                            stringify!($name),
                            <$ty as ParamStore>::TYPE_STRING,
                            Self::$name() as f32,
                            $min as f32,
                            $max as f32,
                            $step as f32
                        );
                    )*
                }

                #[inline(always)]
                pub fn set_param(name: String, val: String) {
                    // trick from akimbo since idents can't go in function names...
                    match name.as_str() {
                        $(
                            stringify!($name) => match val.parse::<$ty>() {
                                    Ok(v) => PARAMS.$name.store(
                                        <$ty as ParamStore>::to_store(v),
                                        Ordering::Relaxed
                                    ),
                                    _ => eprintln!("Could not parse option value!"),
                            },
                        )*
                        _ => eprintln!("Unsupported option command!"),
                    }
                }

                $(
                    #[inline(always)]
                    pub fn $name() -> $ty {
                        let v = PARAMS.$name.load(Ordering::Relaxed);
                        <$ty as ParamStore>::from_store(v)
                    }
                )*
            }
        }

        #[cfg(not(feature = "tune"))]
        mod params {
            use super::Eval;

            pub struct P;

            impl P {
                pub fn init() {}
                pub fn print_options() {}
                pub fn print_params_ob() { eprintln!("SPSA Tuning support not enabled!"); }

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
    big_delta: Eval = {val=1100, min=800, max=2000, step=100},
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
    lmr_base: f32 = {val=0.75, min=0.5, max=1.5, step=0.05},
    lmr_factor: f32 = {val=2.0, min=1.0, max=4.0, step=0.1},
    pawn: Eval = {val=161, min=80, max=250, step=10},
    knight: Eval = {val=446, min=300, max=600, step=15},
    bishop: Eval = {val=464, min=300, max=600, step=15},
    rook: Eval = {val=705, min=500, max=900, step=20},
    queen: Eval = {val=1322, min=900, max=2000, step=50},
];
