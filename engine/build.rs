/// Setup LMR tables which need float math.
use std::{error::Error, fs::File, io::Write, path::PathBuf};

#[cfg(feature = "syzygy")]
fn build_fathom() {
    let cc = &mut cc::Build::new();
    cc.file("./external/fathom/src/tbprobe.c");
    cc.include("./external/fathom/src/");
    cc.define("_CRT_SECURE_NO_WARNINGS", None);
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
        .parse_callbacks(Box::new(bindgen::CargoCallbacks))
        .layout_tests(false)
        .generate()
        .unwrap();

    bindings.write_to_file("./src/syzygy/bindings.rs").unwrap();
}

fn main() -> Result<(), Box<dyn Error>> {
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
