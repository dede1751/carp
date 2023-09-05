/// Utilities to merge and deduplicate data generation files.
/// Code is completely taken from Viri, and adapted to merge an entire directory.
use super::*;
use std::{
    collections::{hash_map::DefaultHasher, BinaryHeap},
    error::Error,
    ffi::OsStr,
    fs::{self, File},
    hash::{Hash, Hasher},
    io::{BufWriter, Read, Write},
    ops::Range,
    path::{Path, PathBuf},
};

use clap::Args;
use itertools::Itertools;

/// Merge and deduplicate dategen files in a given directory.
#[derive(Args)]
pub struct MergeOptions {
    /// Path to the directory containing the datagen files.
    pub path: PathBuf,
}

/// The number of FEN strings in a single deduplication batch.
const CHUNK_SIZE: usize = 10_000_000;

/// A container to optimise the comparison of strings.
/// Only maintains slices to the original buffer.
#[derive(Clone, Eq, Debug)]
struct QuicklySortableString<'a> {
    /// The string to be sorted.
    pub string: &'a str,
    /// The range of the string that we are deduping based on.
    pub view: Range<usize>,
    /// The hash of the substring.
    pub hash: u64,
}

impl<'a> QuicklySortableString<'a> {
    /// Create a new QuicklySortableString from a string slice and an "interesting" range
    pub fn new(string: &'a str, view: Range<usize>) -> Self {
        let mut hasher = DefaultHasher::new();
        let interesting_range = &string[view.clone()];
        interesting_range.hash(&mut hasher);

        Self {
            string,
            view,
            hash: hasher.finish(),
        }
    }

    /// Get a reference to the backing string.
    pub const fn as_str(&self) -> &str {
        self.string
    }
}

/// Implement partial equality trait used for deduping
impl<'a> PartialEq for QuicklySortableString<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
            && self.string[self.view.clone()] == other.string[other.view.clone()]
    }
}

/// Implement total ordering trait used for sorting
impl<'a> Ord for QuicklySortableString<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.hash.cmp(&other.hash).then_with(|| {
            let a = &self.string[self.view.clone()];
            let b = &other.string[other.view.clone()];
            a.cmp(b)
        })
    }
}

/// Implement partial ordering
/// This just mirrors Ord since we already have total ordering
impl<'a> PartialOrd for QuicklySortableString<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// Merge all datagen files in the given directory.
/// This will read all the data into memory, so be careful with dataset size.
pub fn merge<P: AsRef<Path>>(input_dir: P) -> Result<(), Box<dyn Error>> {
    let out = input_dir.as_ref().join(format!(
        "merged_{}.txt",
        chrono::Local::now().format("%d-%m-%Y_%H-%M-%S")
    ));
    println!(
        "\nMerged data is being saved to {WHITE}{}{DEFAULT}",
        out.display()
    );

    // Read all data into a single string buffer
    let read_time = std::time::Instant::now();
    let mut buffer = Vec::new();
    let dir = fs::read_dir(input_dir)?;
    for path in dir {
        let path = path?.path();

        if path.is_file() && path.extension().and_then(OsStr::to_str) == Some("txt") {
            let mb = path.metadata()?.len() as f64 / 1_000_000.0;
            println!(
                "Found {WHITE}{}{DEFAULT}: reading {mb:.0}MB",
                path.display(),
            );

            File::open(path)?.read_to_end(&mut buffer)?;
        }
    }
    let buffer = String::from_utf8(buffer)?;

    // Generate all the QuickSortableString indices for the buffer
    let indices = buffer
        .lines()
        .map(|line| {
            let split_index = 2 + line.bytes().position(|b| b == b' ').unwrap();
            QuicklySortableString::new(line, 0..split_index)
        })
        .collect::<Vec<_>>();
    let initial_size = indices.len();

    // Deduplicate each individual chunk and add it to the heap
    let mut files = BinaryHeap::new();
    for chunk in indices.chunks(CHUNK_SIZE) {
        let mut data = chunk.to_vec();
        data.sort_unstable();
        data.dedup();
        files.push((-(data.len() as i64), data));
    }

    let mb = buffer.len() as f64 / 1_000_000.0;
    let elapsed = read_time.elapsed();
    println!(
        "\n{GREEN}[1/3]{DEFAULT} Read and Hashed {mb:.0}MB total in {}.{:03}s",
        elapsed.as_secs(),
        elapsed.subsec_millis()
    );

    // Iteratively merge the two smallest chunks from the heap
    let merge_time = std::time::Instant::now();
    while files.len() > 1 {
        let chunk_time = std::time::Instant::now();
        let (s1, f1) = files.pop().unwrap();
        let (s2, f2) = files.pop().unwrap();
        let old_size = -s1 - s2;

        // Merge and dedup the two chunks
        let mut res: Vec<QuicklySortableString<'_>> =
            Itertools::merge(f1.into_iter(), f2.into_iter()).collect();
        res.dedup();

        let new_size = res.len() as i64;
        files.push((-new_size, res));

        let elapsed = chunk_time.elapsed();
        println!(
            "\n\t - Merged and Deduped a batch of {} FENs down to {} FENs in {}.{:03}s",
            old_size,
            new_size,
            elapsed.as_secs(),
            elapsed.subsec_millis()
        );
    }

    // Final deduped data is the last item in the heap.
    let data = files.pop().unwrap().1;
    let elapsed = merge_time.elapsed();
    println!(
        "{GREEN}[2/3]{DEFAULT} Deduped {} FENs down to {} FENs in {}.{:03}s",
        initial_size,
        data.len(),
        elapsed.as_secs(),
        elapsed.subsec_millis()
    );

    // Write final deduplicated data to output file.
    let mut output = BufWriter::new(File::create(out)?);
    let write_time = std::time::Instant::now();
    for fen in &data {
        writeln!(output, "{}", fen.as_str())?;
    }

    output.flush()?;
    let elapsed = write_time.elapsed();
    println!(
        "{GREEN}[3/3]{DEFAULT} Wrote {} lines in {}.{:03}s",
        data.len(),
        elapsed.as_secs(),
        elapsed.subsec_millis()
    );

    Ok(())
}
