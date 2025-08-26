/// Setup LMR tables which need float math.
use std::{env, error::Error, fs::File, io::Write, path::PathBuf};

#[cfg(feature = "syzygy")]
fn build_fathom() {
    let cc = &mut cc::Build::new();
    cc.file("./external/fathom/src/tbprobe.c");
    cc.include("./external/fathom/src/");
    cc.define("_CRT_SECURE_NO_WARNINGS", None);

    // From Princhess, compiler seems to not be passing the target correctly.
    let target_cpu = std::env::var("TARGET_CPU").unwrap_or("native".to_string());
    cc.flag(&format!("-march={}", target_cpu));

    cc.flag("-march=native");
    cc.flag("-w");

    // MSVC doesn't support stdatomic.h, so use clang on Windows
    if std::env::consts::OS == "windows" {
        cc.compiler("clang");
    }

    cc.compile("fathom");
}

#[cfg(feature = "syzygy")]
fn generate_fathom_bindings() {
    let bindings = bindgen::Builder::default()
        .header("./external/fathom/src/tbprobe.h")
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        .layout_tests(false)
        .generate()
        .unwrap();

    bindings.write_to_file("./src/syzygy/bindings.rs").unwrap();
}

fn setup_simd_flags() {
    // Notify rustc of custom feature flags.
    println!("cargo:rustc-check-cfg=cfg(simd_avx512)");
    println!("cargo:rustc-check-cfg=cfg(simd_avx2)");
    println!("cargo:rustc-check-cfg=cfg(simd_sse2)");
    println!("cargo:rustc-check-cfg=cfg(simd_neon)");
    println!("cargo:rustc-check-cfg=cfg(simd_none)");

    // Re-run if these env vars change.
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_ARCH");
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_FEATURE");

    let arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    let features = env::var("CARGO_CFG_TARGET_FEATURE").unwrap_or_default();
    let has = |name: &str| features.split(',').any(|f| f == name);

    let mut selected_cfg = "simd_none";
    if arch == "x86_64" {
        if has("avx512f") {
            selected_cfg = "simd_avx512";
        } else if has("avx2") {
            selected_cfg = "simd_avx2";
        } else if has("sse2") {
            selected_cfg = "simd_sse2";
        }
    } else if arch == "aarch64" && has("neon") {
        selected_cfg = "simd_neon";
    }

    println!("cargo:rustc-cfg={selected_cfg}");
}

fn main() -> Result<(), Box<dyn Error>> {
    setup_simd_flags();

    // Build LMR table
    const LMR_BASE: f32 = 0.75;
    const LMR_FACTOR: f32 = 2.0;

    let mut reductions = [[0; 64]; 64];
    for (depth, table) in reductions.iter_mut().enumerate().skip(1) {
        for (move_count, reduction) in table.iter_mut().enumerate().skip(1) {
            *reduction =
                (LMR_BASE + (depth as f32).ln() * (move_count as f32).ln() / LMR_FACTOR) as usize;
        }
    }

    let lmr = unsafe {
        std::slice::from_raw_parts::<u8>(
            reductions.as_ptr().cast::<u8>(),
            64 * 64 * std::mem::size_of::<usize>(),
        )
    };
    File::create(PathBuf::new().join("..").join("bins").join("lmr.bin"))?.write_all(lmr)?;

    #[cfg(feature = "syzygy")]
    {
        build_fathom();
        generate_fathom_bindings();
    }

    Ok(())
}
