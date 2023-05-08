use std::{
    mem::{size_of, transmute},
    sync::atomic::{AtomicU64, Ordering},
};

use crate::chess::{moves::*, zobrist::*};
use crate::engine::{position::*, search_params::*};

/// TTFlag: determines the type of eval stored in the field
#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum TTFlag {
    Lower,
    Upper,
    Exact,
}

/// TTField: uncompressed external representation of tt entries
///
/// Can compress to 128b by using only 3b for the flag and 29 for the move.
/// Mate scores are normalized within the tt for retrieval at different plies: within the tree, we
/// normalize them to the root-distance, while in the tt they are normalized to node-distance
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
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

/// Default empty field to fill table.
impl Default for TTField {
    fn default() -> Self {
        TTField {
            key: 0,
            flag: TTFlag::Upper,
            best_move: NULL_MOVE,
            eval: 0,
            depth: 0,
            age: 0,
        }
    }
}

impl TTField {
    /// Initialize tt entry with normalized score
    pub fn new(
        position: &Position,
        flag: TTFlag,
        best_move: Move,
        mut eval: Eval,
        depth: usize,
        ply: usize,
    ) -> TTField {
        if eval >= MATE_IN_PLY {
            eval += ply as Eval;
        } else if eval <= -MATE_IN_PLY {
            eval -= ply as Eval;
        };

        TTField {
            key: position.board.hash.0,
            best_move,
            depth: depth as u8,
            age: position.age,
            eval: eval as u16,
            flag,
        }
    }

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

    /// Update field contents for search
    pub fn update_data(&mut self, flag: TTFlag, best_move: Move, eval: Eval) {
        self.flag = flag;
        self.best_move = best_move;
        self.eval = eval as u16;
    }
}

/// Actual TTField, compressed down to 16B
#[derive(Debug)]
struct AtomicField(AtomicU64, AtomicU64);

/// Default empty field
impl Default for AtomicField {
    fn default() -> Self {
        AtomicField(AtomicU64::new(0), AtomicU64::new(0))
    }
}

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
}
pub const DEFAULT_SIZE: usize = 256;

// Default to 256 MiB size
impl Default for TT {
    fn default() -> Self {
        TT::new(DEFAULT_SIZE)
    }
}

impl TT {
    pub fn new(mb_size: usize) -> TT {
        let max_size = (mb_size << 20) / size_of::<AtomicField>() + 1;
        let actual_size = max_size.next_power_of_two() / 2; // ensures table size is power of 2
        let bitmask = actual_size as u64 - 1;

        let mut table: Vec<AtomicField> = Vec::with_capacity(actual_size);
        for _ in 0..actual_size {
            table.push(AtomicField::default())
        }

        TT { table, bitmask }
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
    pub fn insert(&self, new: TTField) {
        let tt_index = (new.key & self.bitmask) as usize;
        let old_slot = unsafe { self.table.get_unchecked(tt_index) };
        let old  = old_slot.read_unchecked();

        if  new.age > old.age  // always replace old entries
            || old.depth == 0  // always replace qsearch entries
            || ((old.flag != TTFlag::Exact && (new.flag == TTFlag::Exact || new.depth >= old.depth))
            || (old.flag == TTFlag::Exact && new.flag == TTFlag::Exact && new.depth > old.depth))
        {
            old_slot.write(new);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tt_init() {
        let tt = TT::new(1); // 1 MiB table -> 2^20 / 2^4 = 2^16 slot

        assert_eq!(16, size_of::<AtomicField>());
        assert_eq!(65536, tt.table.len());
    }

    #[test]
    fn test_tt_insert() {
        let tt = TT::default();

        let field1 = TTField {
            key: tt.bitmask,
            flag: TTFlag::Exact,
            best_move: Move(25625038),
            eval: 100,
            depth: 1,
            age: 0,
        };

        let field2 = TTField {
            key: tt.bitmask,
            flag: TTFlag::Exact,
            best_move: Move(25625038),
            eval: 100,
            depth: 2,
            age: 0,
        };

        tt.insert(field1); // insert in empty field
        tt.insert(field2); // replace
        tt.insert(field1); // do not replace

        let target1 = tt.probe(ZHash(field2.key)).unwrap();
        let target2 = tt.probe(ZHash(8));

        assert_eq!(field2, target1);
        assert!(target2.is_none());
    }

    #[test]
    fn test_tt_collision() {
        let tt = TT::new(1);

        let field1 = TTField {
            key: 65537,
            flag: TTFlag::Exact,
            best_move: NULL_MOVE,
            eval: 100,
            depth: 1,
            age: 0,
        };

        let field2 = TTField {
            key: 1,
            flag: TTFlag::Exact,
            best_move: NULL_MOVE,
            eval: 100,
            depth: 2,
            age: 0,
        };

        tt.insert(field1); // insert field 1
        tt.insert(field2); // insert field 2 in same slot as field 1, replacing it
        let new = tt.probe(ZHash(65537)); // check no match on first hash

        assert!(new.is_none());
    }
}
