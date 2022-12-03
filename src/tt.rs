//! # Implements a transposition table to lookup previously searched nodes
//! 
//! 

use std::mem::size_of;

use crate::{
    moves::*,
    evaluation::*,
    zobrist::*,
};

#[derive(Copy, Clone)]
pub enum TTFlag { Exact, Upper, Lower }

/// # TTField -- 20B size
#[derive(Copy, Clone)]
pub struct TTField {
    pub key: ZHash,     // 8B
    best_move: Move,    // 4B
    pub depth: u8,      // 1B
    pub step: u8,       // 1B
    value: Eval,        // 4B
    pub flag: TTFlag,   // 1B
}

/// Default empty field to fill table.
impl Default for TTField {
    fn default() -> Self {
        TTField {
            key: NULL_HASH,
            best_move: NULL_MOVE,
            depth: 0,
            step: 0, // (iterative deepening "step" count)
            value: 0,
            flag: TTFlag::Exact,
        }
    }
}

impl TTField {
    pub fn new(
        key: ZHash,
        best_move: Move,
        depth: usize,
        step: u8,
        mut value: Eval,
        flag: TTFlag,
        ply: usize,
    ) -> TTField {
        // normalize mate scores
        if value > MATE {
            value += ply as Eval;
        } else if value < -MATE {
            value -= ply as Eval;
        };

        TTField {
            key,
            best_move,
            depth: depth as u8,
            step,
            value,
            flag,
        }
    }
    /// Returns best move if found
    pub fn get_best_move(&self) -> Option<Move> {
        if self.best_move != NULL_MOVE {
            Some(self.best_move)
        } else {
            None
        }
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
    pub fn get_value(&self, ply: usize) -> Eval {
        if self.value > MATE {
            self.value - ply as Eval
        } else if self.value < -MATE {
            self.value + ply as Eval
        } else {
            self.value
        }
    }
}

pub struct TT {
    table: Vec<TTField>,
    pub bitmask: u64,
}

impl Default for TT {
    fn default() -> Self {
        TT::new(256)
    }
}

impl TT {
    pub fn new(mb_size: usize) -> TT {
        let max_size: usize = mb_size * 1024 * 1024 / size_of::<TTField>() + 1;
        let actual_size: usize = max_size.next_power_of_two() / 2;
        let bitmask: u64 = actual_size as u64 - 1;

        let table: Vec<TTField> = vec![TTField::default(); actual_size];

        TT { table, bitmask }
    }

    /// Probe first 1000 buckets for non-null entries
    pub fn usage(&self) -> f64 {
        let mut count: f64 = 0f64;

        for field in &self.table[0..1000] {
            if field.key != NULL_HASH { count += 1f64; }
        }

        count / 1000f64
    }

    /// Probe tt for entry
    /// 
    /// UB: since bitmask and tables cannot be externally modified, it is impossible for get
    ///     unchecked to fail.
    pub fn probe(&self, hash: ZHash) -> Option<&TTField> {
        let tt_index: usize = (hash.0 & self.bitmask) as usize;
        let field: &TTField = unsafe { self.table.get_unchecked(tt_index) };

        if field.key == hash {
            Some(field)
        } else {
            None
        }
    }

    /// Insert entry in appropriate tt field.
    /// Uses highest depth replacement scheme, except for older entries which are always replaced
    pub fn insert(&mut self, entry: TTField) {
        let tt_index: usize = (entry.key.0 & self.bitmask) as usize;
        let field: &mut TTField = unsafe { self.table.get_unchecked_mut(tt_index) };
        
        if entry.key != field.key &&             // no table collision
            (entry.step  >  field.step    ||  // entry is newer
             entry.depth >= field.depth)      // entry is deeper
        {
            *field = entry;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transposition_table_init() {
        let tt: TT = TT::default();
        
        assert_eq!(24, size_of::<TTField>());
        assert_eq!(512 * 1024 * 1024 / size_of::<TTField>(), tt.table.len());
    }

    #[test]
    fn test_transposition_table_insert() {
        let mut tt: TT = TT::default();
        
        let entry: TTField = TTField { 
            key: ZHash(tt.bitmask),
            best_move: NULL_MOVE,
            depth: 1,
            step: 0,
            value: 100,
            flag: TTFlag::Exact,
        };

        tt.insert(entry);

        let entry = tt.probe(ZHash(tt.bitmask));
        assert!(entry.is_some());
    }
}