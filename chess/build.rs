/// Build the Magic attack lookup tables and store them in the bins folder.
mod build_deps;

use std::{error::Error, fs::File, mem::size_of, path::PathBuf};

use build_deps::{
    magics::{Magics, ATTACK_COUNT},
    masks::{Bishop, Rook},
    types::BitBoard,
    write_to_file_bin,
};

fn main() -> Result<(), Box<dyn Error>> {
    let mut attacks = [BitBoard::EMPTY; ATTACK_COUNT];
    let bishop_magics = Magics::<Bishop>::init(&mut attacks);
    let rook_magics = Magics::<Rook>::init(&mut attacks);

    let out_dir = PathBuf::new().join("..").join("bins");

    bishop_magics.write_to_file(File::create(out_dir.join("bishop_magics.bin"))?)?;
    rook_magics.write_to_file(File::create(out_dir.join("rook_magics.bin"))?)?;
    write_to_file_bin(
        &mut File::create(out_dir.join("attacks.bin"))?,
        &attacks,
        size_of::<[BitBoard; ATTACK_COUNT]>(),
    )?;

    Ok(())
}
