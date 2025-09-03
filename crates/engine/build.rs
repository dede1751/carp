/// Setup LMR tables which need float math.
use std::{collections::HashMap, env, error::Error, fs::File, io::Write, path::{Path, PathBuf}};

fn setup_simd_flags() {
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_ARCH");
    println!("cargo:rerun-if-env-changed=CARGO_CFG_TARGET_FEATURE");

    // Notify rustc of custom feature flags.
    let mut supported_simd = HashMap::new();
    supported_simd.insert("x86_64", vec!["avx512vnni", "avx512f", "avxvnni", "avx2"]);
    supported_simd.insert("aarch64", vec!["neon"]);

    for arch_simd in supported_simd.values() {
        for simd in arch_simd.iter() {
            println!("cargo:rustc-check-cfg=cfg(simd_{})", simd);
        }
    }
    println!("cargo:rustc-check-cfg=cfg(simd_none)");

    let arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap_or_default();
    let features = env::var("CARGO_CFG_TARGET_FEATURE").unwrap_or_default();
    let has = |name: &str| features.split(',').any(|f| f == name);
    supported_simd
        .get(arch.as_str())
        .and_then(|simd_list| simd_list.iter().find(|&simd| has(simd)))
        .map(|&simd| println!("cargo:rustc-cfg=simd_{}", simd))
        .unwrap_or_else(|| println!("cargo:rustc-cfg=simd_none"));
}

fn setup_evalfile(bin_path: &Path) -> std::io::Result<()> {
    let default_net_path = bin_path.join("net.bin");
    println!("cargo:rerun-if-env-changed=EVALFILE");
    println!("cargo:rerun-if-changed={}", default_net_path.display());

    if let Ok(evalfile) = env::var("EVALFILE") {
        std::fs::copy(evalfile, default_net_path)?;
    }

    Ok(())
}

fn build_lmr(bin_path: &Path) -> std::io::Result<()> {
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
    
    File::create(bin_path.join("lmr.bin"))?.write_all(lmr)
}

#[cfg(feature = "syzygy")]
fn build_fathom() {
    let cc = &mut cc::Build::new();
    cc.file("./external/fathom/src/tbprobe.c");
    cc.include("./external/fathom/src/");
    cc.define("_CRT_SECURE_NO_WARNINGS", None);

    // From Princhess, compiler seems to not be passing the target correctly.
    let target_cpu = std::env::var("TARGET_CPU").unwrap_or("native".to_string());
    cc.flag(format!("-march={}", target_cpu));

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

fn main() -> Result<(), Box<dyn Error>> {
    let bin_path = PathBuf::new().join("..").join("..").join("bins");

    setup_simd_flags();
    setup_evalfile(&bin_path)?;
    build_lmr(&bin_path)?;

    #[cfg(feature = "syzygy")]
    {
        build_fathom();
        generate_fathom_bindings();
    }

    Ok(())
}
