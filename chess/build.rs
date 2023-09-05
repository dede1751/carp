/// Build the Magic attack lookup tables and store them in the bins folder.
mod build_deps;

use std::{error::Error, fs::File, mem::size_of, path::PathBuf};

use build_deps::{
    magics::{Magics, ATTACK_COUNT},
    sliders::{Bishop, Rook, Sliding},
    types::{BitBoard, Square},
    write_to_file_bin,
};

fn main() -> Result<(), Box<dyn Error>> {
    let out_dir = PathBuf::new().join("..").join("bins");

    // Build attack lookup tables
    let mut attacks = [BitBoard::EMPTY; ATTACK_COUNT];
    let bishop_magics = Magics::<Bishop>::init(&mut attacks);
    let rook_magics = Magics::<Rook>::init(&mut attacks);

    bishop_magics.write_to_file(File::create(out_dir.join("bishop_magics.bin"))?)?;
    rook_magics.write_to_file(File::create(out_dir.join("rook_magics.bin"))?)?;
    unsafe {
        write_to_file_bin(
            &mut File::create(out_dir.join("attacks.bin"))?,
            &attacks,
            size_of::<[BitBoard; ATTACK_COUNT]>(),
        )?
    };

    // Build Between lookup table
    let mut between = [[BitBoard::EMPTY; Square::COUNT]; Square::COUNT];
    Bishop::gen_between(&mut between);
    Rook::gen_between(&mut between);
    unsafe {
        write_to_file_bin(
            &mut File::create(out_dir.join("between.bin"))?,
            &between,
            size_of::<[[BitBoard; Square::COUNT]; Square::COUNT]>(),
        )?
    };

    Ok(())
}
