use std::{
    mem::{size_of, transmute},
    sync::atomic::{AtomicU64, Ordering},
};

use crate::chess::{moves::*, zobrist::*};
use crate::engine::search_params::*;

/// TTFlag: determines the type of eval stored in the field
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash, Default)]
pub enum TTFlag {
    #[default]
    None,
    Lower,
    Upper,
    Exact,
}

/// TTField: uncompressed external representation of tt entries
///
/// Can compress to 128b by using only 3b for the flag and 29 for the move.
/// Mate scores are normalized within the tt for retrieval at different plies: within the tree, we
/// normalize them to the root-distance, while in the tt they are normalized to node-distance
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash, Default)]
pub struct TTField {
    key: u64,        // 8B
    flag: TTFlag,    // 1B -- only the rightmost 3 bits are actually of note
    best_move: Move, // 4B -- only the rightmost 28 bits are actually of note
    eval: u16,       // 2B
    depth: u8,       // 1B
    age: u8,         // 1B
}

// Masks for the second u64
const DEPTH_OFFSET: u64 = 8;
const EVAL_OFFSET: u64 = 16;
const MOVE_OFFSET: u64 = 32;
const FLAG_OFFSET: u64 = 61;
const AGE_MASK: u64 = 0x00000000000000FF; // first byte
const DEPTH_MASK: u64 = 0x000000000000FF00; // second byte
const EVAL_MASK: u64 = 0x00000000FFFF0000; // third/fourth byte
const MOVE_MASK: u64 = 0x1FFFFFFF00000000; // last four bytes except for final 3 bits

/// Convert from external field to compressed internal
impl From<TTField> for (u64, u64) {
    fn from(field: TTField) -> Self {
        let data: u64 = field.age as u64
            | (field.depth as u64) << DEPTH_OFFSET
            | (field.eval as u64) << EVAL_OFFSET
            | (field.best_move.0 as u64) << MOVE_OFFSET
            | (field.flag as u64) << FLAG_OFFSET;

        (field.key ^ data, data)
    }
}

/// Convert from compressed internal to external
impl From<(u64, u64)> for TTField {
    fn from((key, data): (u64, u64)) -> Self {
        let age = (data & AGE_MASK) as u8;
        let depth = ((data & DEPTH_MASK) >> DEPTH_OFFSET) as u8;
        let eval = ((data & EVAL_MASK) >> EVAL_OFFSET) as u16;
        let best_move = Move(((data & MOVE_MASK) >> MOVE_OFFSET) as u32);
        let flag = unsafe { transmute((data >> FLAG_OFFSET) as u8) };

        TTField {
            key,
            flag,
            best_move,
            eval,
            depth,
            age,
        }
    }
}

impl TTField {
    /// Returns entry depth
    pub fn get_depth(&self) -> usize {
        self.depth as usize
    }

    /// Returns entry flag
    pub fn get_flag(&self) -> TTFlag {
        self.flag
    }

    /// Returns best move
    pub fn get_move(&self) -> Move {
        self.best_move
    }

    /// Gets eval while normalizing mate scores
    pub fn get_eval(&self, ply: usize) -> Eval {
        let eval = self.eval as Eval;
        let ply = ply as Eval;

        if eval >= MATE_IN_PLY {
            eval - ply
        } else if eval <= -MATE_IN_PLY {
            eval + ply
        } else {
            eval
        }
    }
}

/// Actual TTField, compressed down to 16B
#[derive(Debug, Default)]
struct AtomicField(AtomicU64, AtomicU64);

impl AtomicField {
    /// Atomic read from table to a TTField, checking that the checksum is correct
    fn read(&self, checksum: u64) -> Option<TTField> {
        let key = self.0.load(Ordering::Relaxed);
        let data = self.1.load(Ordering::Relaxed);

        if key ^ checksum == data {
            Some(TTField::from((checksum, data)))
        } else {
            None
        }
    }

    /// Atomic read, returns a TTField with a scrambled key
    fn read_unchecked(&self) -> TTField {
        let key = self.0.load(Ordering::Relaxed);
        let data = self.1.load(Ordering::Relaxed);

        TTField::from((key, data))
    }

    /// Atomic write to table from a tt field struct
    fn write(&self, entry: TTField) {
        let (key, data) = entry.into();

        self.0.store(key, Ordering::Relaxed);
        self.1.store(data, Ordering::Relaxed);
    }
}

/// Main transposition table with 16B atomic entries
/// https://www.chessprogramming.org/Shared_Hash_Table
pub struct TT {
    table: Vec<AtomicField>,
    bitmask: u64,
    age: u8,
}
pub const DEFAULT_SIZE: usize = 16;

// Default to 16 MiB size
impl Default for TT {
    fn default() -> Self {
        let mut tt = TT {
            table: Vec::new(),
            bitmask: 0,
            age: 0,
        };
        tt.resize(DEFAULT_SIZE);

        tt
    }
}

impl TT {
    /// Resize the tt to the given size in MiB
    pub fn resize(&mut self, mb_size: usize) {
        let max_size = (mb_size << 20) / size_of::<AtomicField>() + 1;
        let actual_size = max_size.next_power_of_two() / 2; // ensures table size is power of 2

        self.bitmask = actual_size as u64 - 1;
        self.table = Vec::with_capacity(actual_size);
        for _ in 0..actual_size {
            self.table.push(AtomicField::default())
        }
    }

    /// Reset the tt to empty entries
    pub fn clear(&mut self) {
        self.age = 0;
        for entry in self.table.iter_mut() {
            *entry = AtomicField::default();
        }
    }

    /// Increase current age. Entries with older age will be overwritten more easily.
    pub fn increment_age(&mut self) {
        self.age = self.age.wrapping_add(1);
    }

    /// Probe tt for entry
    ///
    /// UB: since bitmask and tables cannot be externally modified, it is impossible for get
    ///     unchecked to fail.
    pub fn probe(&self, hash: ZHash) -> Option<TTField> {
        let tt_index = (hash.0 & self.bitmask) as usize;
        unsafe { self.table.get_unchecked(tt_index).read(hash.0) }
    }

    /// Insert entry in appropriate tt field.
    ///
    /// Uses highest depth + aging for replacement, but takes special care of different flag types
    /// to maintain the pv within the transposition table:
    ///     - highest priority is given to entry age
    ///     - second highest is given to Exact entries: they only get replaced by better exact ones
    ///     - inexact entries get overwritten by exact entries or entries at the same depth. this
    ///       is because in the search_root function we may replace the same entry many times.
    ///
    /// Conditions are explained here:
    /// https://stackoverflow.com/questions/37782131/chess-extracting-the-principal-variation-from-the-transposition-table
    #[rustfmt::skip]
    pub fn insert(
        &self,
        key: ZHash,
        flag: TTFlag,
        best_move: Move,
        eval: Eval,
        depth: usize,
        ply: usize
    ) {
        let tt_index = (key.0 & self.bitmask) as usize;
        let old_slot = unsafe { self.table.get_unchecked(tt_index) };
        let old  = old_slot.read_unchecked();

        if  self.age > old.age  // always replace old entries
            || old.depth == 0  // always replace qsearch/empty entries
            || ((old.flag != TTFlag::Exact && (flag == TTFlag::Exact || depth >= old.depth as usize))
            || (old.flag == TTFlag::Exact && flag == TTFlag::Exact && depth > old.depth as usize))
        {
            let eval = if eval >= MATE_IN_PLY {
                (eval + ply as Eval) as u16
            } else if eval <= -MATE_IN_PLY {
                (eval - ply as Eval) as u16
            } else {
                eval as u16
            };

            let new = TTField {
                key: key.0,
                flag,
                best_move,
                eval,
                depth: depth as u8,
                age: self.age
            };

            old_slot.write(new);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tt_init() {
        let mut tt = TT::default();
        tt.resize(1); // 1 MiB table -> 2^20 / 2^4 = 2^16 slot

        assert_eq!(16, size_of::<AtomicField>());
        assert_eq!(65536, tt.table.len());
    }

    #[test]
    fn test_tt_insert() {
        let tt = TT::default();
        let z = ZHash(tt.bitmask);

        tt.insert(z, TTFlag::Exact, Move(25625038), 100, 1, 0); // insert in empty field
        tt.insert(z, TTFlag::Exact, Move(25625038), 100, 2, 0); // replace
        tt.insert(z, TTFlag::Exact, Move(25625038), 100, 1, 0); // do not replace

        let target1 = tt.probe(z).unwrap();
        let target2 = tt.probe(ZHash(8));

        assert_eq!(2, target1.get_depth());
        assert!(target2.is_none());
    }

    #[test]
    fn test_tt_collision() {
        let mut tt = TT::default();
        tt.resize(1);

        // 65537 is 2^16 + 1, so it will collide with 1. checksum should prevent reading
        tt.insert(ZHash(65537), TTFlag::Exact, NULL_MOVE, 100, 1, 0); // insert field 1
        tt.insert(ZHash(1), TTFlag::Exact, NULL_MOVE, 100, 2, 0); // insert field 2 in same slot as field 1, replacing it

        let new = tt.probe(ZHash(65537)); // check no match on first hash
        assert!(new.is_none());
    }
}
