//! # Implements a transposition table to lookup previously searched nodes
//! 
//! The transposition table uses internal Atomic entries to reduce memory footprint and future
//! multithreading capabilities. The internal representation 

use std::mem::{
    transmute,
    size_of,
};
use std::sync::atomic::{
    AtomicU64,
    Ordering,
};

use crate::{
    moves::*,
    evaluation::*,
    zobrist::*,
};

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum TTFlag { Lower, Upper, Exact }

/// # TTField -- 17B (136b) total, aligns at 24B
/// 
/// Can compress to 131b by using only 3b for the flag. Then reducing key from 64b to 61 brings
/// us down to 128b or 16B for perfect alignment.
/// 
/// Implementation note: saving value as an i16 within the TTField led to the most absolutely insane
/// bug I've ever had to find. Apparently i16 converted to u64 gets sign-extended before conversion,
/// hence within into<TTEntry> the move would actually be overwritten by the eval and garbage woud;
/// propagate.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub struct TTField {
    key: u64,        // 8B -- Only rightmost 61b
    best_move: Move, // 4B
    depth: u8,       // 1B
    age: u8,         // 1B
    value: u16,      // 2B
    flag: TTFlag,    // 1B
}

// Masks for the first u64
const FLAG_OFFSET: u64 = 61;
const KEY_MASK : u64 = 0x1FFFFFFFFFFFFFFF; // first 61 bits
const FLAG_MASK: u64 = 0xE000000000000000; // last 3 bits

// Masks for the second u64
const DEPTH_OFFSET: u64 = 8;
const EVAL_OFFSET : u64 = 16;
const MOVE_OFFSET : u64 = 32;
const AGE_MASK  : u64 = 0x00000000000000FF; // first byte
const DEPTH_MASK: u64 = 0x000000000000FF00; // second byte
const EVAL_MASK : u64 = 0x00000000FFFF0000; // third/fourth byte
const MOVE_MASK : u64 = 0xFFFFFFFF00000000; // last four bytes

// Convert from external field to compressed internal
impl Into<(u64, u64)> for TTField {
    fn into(self) -> (u64, u64) {
        let a: u64 = 
            self.key                                 |
            (self.flag as u64) << FLAG_OFFSET;
    
        let b: u64 = 
            self.age as u64                          |
            (self.depth as u64) << DEPTH_OFFSET      |
            (self.value as u64) << EVAL_OFFSET       |
            (self.best_move.0 as u64) << MOVE_OFFSET;

        (a, b)
    }
}

// Convert from compressed internal atomic to external
impl From<(u64, u64)> for TTField {
    fn from((a, b): (u64, u64)) -> Self {
        let key: u64 = a & KEY_MASK;
        let flag: TTFlag = unsafe { transmute((a >> FLAG_OFFSET) as u8) };

        let age: u8 = (b & AGE_MASK) as u8; 
        let depth: u8 = ((b & DEPTH_MASK) >> DEPTH_OFFSET) as u8;
        let value: u16 = ((b & EVAL_MASK) >> EVAL_OFFSET) as u16;
        let best_move: Move = Move(((b & MOVE_MASK) >> MOVE_OFFSET) as u32);

        TTField {
            key,
            best_move,
            depth,
            age,
            value,
            flag
        }
    }
}

/// Default empty field to fill table.
impl Default for TTField {
    fn default() -> Self {
        TTField {
            key: 0,
            best_move: NULL_MOVE,
            depth: 0,
            age: 0, // (iterative deepening "step" count)
            value: 0,
            flag: TTFlag::Lower,
        }
    }
}

impl TTField {
    pub fn new(
        key: ZHash,
        best_move: Move,
        depth: u8,
        age: u8,
        mut value: Eval,
        flag: TTFlag,
        ply: u8,
    ) -> TTField {
        // Keep only rightmost 61 bits of key.
        let reduced_key: u64 = key.0 & KEY_MASK;

        // normalize mate scores
        if value > MATE_SCORE {
            value += ply as Eval;
        } else if value < -MATE_SCORE {
            value -= ply as Eval;
        };

        TTField {
            key: reduced_key,
            best_move,
            depth,
            age,
            value: value as u16,
            flag,
        }
    }

    /// Returns entry depth
    #[inline]
    pub fn get_depth(&self) -> u8 {
        self.depth
    }

    /// Returns entry flag
    #[inline]
    pub fn get_flag(&self) -> TTFlag {
        self.flag
    }

    /// Returns best move
    pub fn get_move(&self) -> Move {
        self.best_move
    }

    /// # Gets value while normalizing mate scores:
    /// 
    /// Tree --> Mate scores are relative to root-distance
    ///          When mating the opponent, score is     MATE - mate_distance_from_root
    ///          When being mated, score is            -MATE + mate_distance_from_root
    /// TT   --> Mate scores are relative to node-distance
    ///          When mating the opponent, score is     MATE - mate_distance_from_node
    ///          When being mated, score is            -MATE + mate_distance_from_node
    /// 
    /// mate_distance_from_root = ply + mate_distance_from_node
    pub fn get_value(&self, ply: u8) -> Eval {
        let eval = self.value as Eval;

        if is_mate(eval) {
            eval - ply as Eval
        } else if is_mated(eval) {
            eval + ply as Eval
        } else {
            eval
        }
    }
}

/// Actual TTField, compressed down to 16B
/// This is atomic for future Lazy-SMP implementation
#[derive(Debug)]
struct AtomicField (AtomicU64, AtomicU64);

/// Default empty field
impl Default for AtomicField {
    fn default() -> Self {
        AtomicField(AtomicU64::new(0), AtomicU64::new(0))
    }
}

impl AtomicField {
    /// Atomic read from table to a tt field struct
    #[inline]
    fn read(&self) -> TTField {
        (
            self.0.load(Ordering::Relaxed),
            self.1.load(Ordering::Relaxed)
        ).into()
    }

    /// Atomic write to table from a tt field struct
    #[inline]
    fn write(&self, entry: TTField) {
        let (a, b): (u64, u64) = entry.into();

        self.0.store(a, Ordering::Relaxed);
        self.1.store(b, Ordering::Relaxed);
    }
}

/// Main transposition table with 16B atomic entries
pub struct TT {
    table: Vec<AtomicField>,
    bitmask: u64,
}

// Default to 256 MiB size
impl Default for TT {
    fn default() -> Self {
        TT::new(256)
    }
}

impl TT {
    pub fn new(mb_size: usize) -> TT {
        let max_size: usize = (mb_size << 20) / size_of::<AtomicField>() + 1;
        let actual_size: usize = max_size.next_power_of_two() / 2; // ensures table size is power of 2
        let bitmask: u64 = actual_size as u64 - 1;

        let mut table: Vec<AtomicField> = Vec::with_capacity(actual_size);
        for _ in 0..actual_size { table.push(AtomicField::default()) }

        TT { table, bitmask }
    }

    /// Probe tt for entry
    /// 
    /// UB: since bitmask and tables cannot be externally modified, it is impossible for get
    ///     unchecked to fail.
    pub fn probe(&self, hash: ZHash) -> Option<TTField> {
        let tt_index: usize = (hash.0 & self.bitmask) as usize;
        let field: TTField = unsafe { self.table.get_unchecked(tt_index).read() };

        if field.key == (hash.0 & KEY_MASK) {
            Some(field)
        } else {
            None
        }
    }

    /// Insert entry in appropriate tt field.
    /// 
    /// Uses highest depth + aging for replacement, but takes special care of different flag types
    /// to maintain the pv within the transposition table. Conditions are explained here:
    /// https://stackoverflow.com/questions/37782131/chess-extracting-the-principal-variation-from-the-transposition-table
    /// 
    /// Since insertion condition evaluation is not atomic, multiple threads can overrite each
    /// other's entries if evaluating conditions at the same time. Since the philosophy of lazy-SMP
    /// is no sycn overhead, we happily ignore these race conditions. This requires collecting the
    /// best move at the root node of the search since it may get overwritten in the table.
    pub fn insert(&self, new: TTField) {
        let tt_index: usize = (new.key & self.bitmask) as usize;
        let old_field: &AtomicField = unsafe { self.table.get_unchecked(tt_index) };
        let old: TTField = old_field.read();
        
        if (old.flag != TTFlag::Exact &&                        // old is not exact bound
            (new.flag == TTFlag::Exact || (new.age > old.age || new.depth >= old.depth))) // new is exact or better

            ||
        
            (old.flag == TTFlag::Exact &&                     // old is exact bound
            new.flag == TTFlag::Exact &&                      // new is exact bound
            (new.age > old.age || new.depth >= old.depth))    // new is a better entry
        {
            old_field.write(new);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tt_field_conversion() {
        let field: TTField = TTField::new(
            ZHash(5), NULL_MOVE, 4, 1, 50, TTFlag::Upper, 0
        );

        let x: (u64, u64) = field.into();
        let new_field: TTField = x.into();
        println!("\n{:?}\n{:?}", field, new_field);

        assert_eq!(field, new_field);
    }

    #[test]
    fn test_tt_init() {
        let tt: TT = TT::new(1); // 1 MiB table -> 2^20 / 2^4 = 2^16 slot
        
        assert_eq!(16, size_of::<AtomicField>());
        assert_eq!(65536, tt.table.len());
    }

    #[test]
    fn test_tt_insert() {
        let tt: TT = TT::default();
        
        let field1: TTField = TTField::new(
            ZHash(tt.bitmask), Move(25625038), 1, 0, -100, TTFlag::Exact, 0,
        );
        
        let field2: TTField = TTField::new(
            ZHash(tt.bitmask), Move(25625038), 2, 0, -100, TTFlag::Exact, 0,
        );

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
        let tt: TT = TT::new(1);
        let h1: ZHash = ZHash(65537);
        let h2: ZHash = ZHash(1);

        let field1: TTField = TTField::new(
            h1, NULL_MOVE, 1, 0, 100, TTFlag::Exact, 0,
        );

        let field2: TTField = TTField::new(
            h2, NULL_MOVE, 2, 0, 100, TTFlag::Exact, 0,
        );

        tt.insert(field1); // insert field 1
        tt.insert(field2); // insert field 2 in same slot as field 1, replacing it
        let new = tt.probe(h1); // check that probing h1 does not match
        
        assert!(new.is_none());
    }
}