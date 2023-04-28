/// Utilities to merge and deduplicate data generation files.
/// Code is completely taken from Viri, and adapted to merge an entire directory.
use std::{
    collections::hash_map::DefaultHasher,
    error::Error,
    ffi::OsStr,
    fs::{self, File},
    io::{self, BufWriter, Read, Write},
    ops::Range,
    path::Path,
};

use super::*;

/// A container to optimise the comparison of strings.
struct QuicklySortableString<'a> {
    /// The string to be sorted.
    pub string: &'a str,
    /// The substring that we are deduping based on.
    pub view: Range<usize>,
    /// The hash of the substring.
    pub hash: u64,
}

impl<'a> QuicklySortableString<'a> {
    pub fn new(string: &'a str, view: Range<usize>) -> Self {
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        let interesting_range = &string[view.clone()];
        interesting_range.hash(&mut hasher);
        Self {
            string,
            view,
            hash: hasher.finish(),
        }
    }

    /// Compare the substring of the two strings.
    pub fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.hash.cmp(&other.hash).then_with(|| {
            let a = &self.string[self.view.clone()];
            let b = &other.string[other.view.clone()];
            a.cmp(b)
        })
    }

    /// Get a reference to the backing string.
    pub const fn as_str(&self) -> &str {
        self.string
    }
}

/// Merge all datagen files in the given directory.
/// This will read all the data into memory, so be careful with dataset size.
pub fn merge<P1: AsRef<Path>>(input_dir: P1) -> Result<(), Box<dyn Error>> {
    let output = input_dir.as_ref().join("merged.txt");
    let mut buffer = Vec::new();
    let start_time = std::time::Instant::now();
    let dir = fs::read_dir(&input_dir)?;

    println!(
        "\nMerged data is being saved to {WHITE}{}{DEFAULT}",
        output.display()
    );

    // read all txt files in the data dir into memory
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

    let all_data = String::from_utf8(buffer)?;
    let mb = all_data.len() as f64 / 1_000_000.0;
    let elapsed = start_time.elapsed();
    println!(
        "\n{GREEN}[1/4]{DEFAULT} Read {mb:.0}MB total in {}.{:03}s",
        elapsed.as_secs(),
        elapsed.subsec_millis()
    );

    Ok(dedup_inner(output, &all_data)?)
}

fn dedup_inner<P: AsRef<Path>>(output_file: P, all_data: &str) -> Result<(), io::Error> {
    let mut output = BufWriter::new(File::create(output_file)?);
    let start_time = std::time::Instant::now();

    // split all data into QSS, only sorting over the board portion of the fen
    let mut data = all_data
        .lines()
        .map(|line| {
            let split_index = line.bytes().position(|b| b == b' ').unwrap();
            QuicklySortableString::new(line, 0..(split_index + 2))
        })
        .collect::<Vec<_>>();

    let n = data.len();
    let elapsed = start_time.elapsed();
    println!(
        "{GREEN}[2/4]{DEFAULT} Hashed {n} lines in {}.{:03}s",
        elapsed.as_secs(),
        elapsed.subsec_millis()
    );

    // sort and dedup all data based on board hash
    let start_time = std::time::Instant::now();
    data.sort_unstable_by(QuicklySortableString::cmp);
    data.dedup_by(|a, b| a.cmp(b) == std::cmp::Ordering::Equal);
    let n = data.len();
    let elapsed = start_time.elapsed();
    println!(
        "{GREEN}[3/4]{DEFAULT} Sorted and deduped down to {n} lines in {}.{:03}s",
        elapsed.as_secs(),
        elapsed.subsec_millis()
    );

    // write deduplicated data to output file.
    let start_time = std::time::Instant::now();
    for line in data {
        let line = line.as_str();
        writeln!(output, "{line}")?;
    }

    let res = output.flush();
    let elapsed = start_time.elapsed();
    println!(
        "{GREEN}[4/4]{DEFAULT} Wrote {n} lines in {}.{:03}s",
        elapsed.as_secs(),
        elapsed.subsec_millis()
    );
    res
}
