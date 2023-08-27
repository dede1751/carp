use std::{error::Error, fs::File, io::Write, path::PathBuf};

fn main() -> Result<(), Box<dyn Error>> {
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

    Ok(())
}
