pub mod magics;
pub mod masks;
pub mod types;

use std::{error::Error, fs::File, io::Write};

/// Write a slice of arbitrary data to a binary file.
/// SAFETY: refer to std::slice::from_raw_parts.
///     Essentially, the data slice must not be mutated throughout lifetime 'a, its contents must be
///     properly aligned and valid for the type T, and the size parameter should be consistent with
///     the slice.
pub fn write_to_file_bin<'a, T>(
    file: &'a mut File,
    data: &[T],
    size: usize,
) -> Result<(), Box<dyn Error>> {
    let byte_slice =
        unsafe { std::slice::from_raw_parts::<'a, u8>(data.as_ptr().cast::<u8>(), size) };
    file.write_all(byte_slice)?;
    Ok(())
}
