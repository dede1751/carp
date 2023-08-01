use crate::chess::{board::*, moves::*, piece::*};
use crate::engine::{clock::*, history_table::*, search_params::*, tt::*};

/// Information only relevant within the search tree (thread local)
pub struct SearchInfo<'a> {
    // Structures used by the search
    pub clock: Clock,
    pub tt: &'a TT,

    // Search stacks
    search_stack: [(Piece, Move, usize); MAX_DEPTH],
    pub eval_stack: [Eval; MAX_DEPTH],
    pub excluded: [Option<Move>; MAX_DEPTH],

    // Move ordering
    pub killer_moves: [[Move; 2]; MAX_DEPTH],
    history: HistoryTable,
    counter_moves: DoubleHistoryTable,
    followup_moves: DoubleHistoryTable,

    // Search stats
    pub nodes: u64,
    pub seldepth: usize,
    pub ply: usize,
    pub ply_from_null: usize,

    // End of search
    pub best_move: Move,
    pub stop: bool,
}

/// Search functionalities
impl<'a> SearchInfo<'a> {
    /// Create a new SearchInfo struct with the given TT and time control
    /// All other fields are initialized as empty.
    pub fn new(tt: &'a TT, clock: Clock) -> SearchInfo {
        SearchInfo {
            clock,
            tt,

            search_stack: [(Piece::WP, NULL_MOVE, 0); MAX_DEPTH],
            eval_stack: [0; MAX_DEPTH],
            excluded: [None; MAX_DEPTH],

            killer_moves: [[NULL_MOVE; 2]; MAX_DEPTH],
            history: HistoryTable::default(),
            counter_moves: DoubleHistoryTable::default(),
            followup_moves: DoubleHistoryTable::default(),

            nodes: 0,
            seldepth: 0,
            ply: 0,
            ply_from_null: 0,

            best_move: NULL_MOVE,
            stop: false,
        }
    }

    /// Push a non-null move to the search stack
    pub fn push_move(&mut self, piece: Piece, m: Move) {
        self.search_stack[self.ply] = (piece, m, self.ply_from_null);
        self.ply += 1;
        self.ply_from_null += 1;
        self.nodes += 1;
    }

    /// Push a null move to the search stack
    pub fn push_null(&mut self) {
        self.search_stack[self.ply] = (Piece::WP, NULL_MOVE, self.ply_from_null);
        self.ply += 1;
        self.ply_from_null = 0;
        self.nodes += 1;
    }

    /// Pop a move from the search stack (panics if called at ply 0)
    pub fn pop_move(&mut self) {
        self.ply -= 1;
        self.ply_from_null = self.search_stack[self.ply].2;
    }

    /// Upon a fail-high, update killer and history tables.
    pub fn update_tables(&mut self, best: Move, depth: usize, side: Color, searched: Vec<Move>) {
        if best != self.killer_moves[self.ply][0] {
            self.killer_moves[self.ply][1] = self.killer_moves[self.ply][0];
            self.killer_moves[self.ply][0] = best;
        }

        // Score histories
        let bonus = history_bonus(depth);

        self.history.update(bonus, best, side, &searched);

        if let Some((p, m, _)) = self.get_previous_entry(1) {
            self.counter_moves.update(bonus, best, p, m.get_tgt(), &searched);
        }

        if let Some((p, m, _)) = self.get_previous_entry(2) {
            self.followup_moves.update(bonus, best, p, m.get_tgt(), &searched);
        }
    }

    /// Assign history scores to movelist slices
    pub fn assign_history_scores(&self, side: Color, moves: &[Move], scores: &mut [i32]) {
        for i in 0..moves.len() {
            scores[i] = self.history.get_score(moves[i], side);
        }

        if let Some((prev_p, prev_m, _)) = self.get_previous_entry(1) {
            for j in 0..moves.len() {
                scores[j] += self.counter_moves.get_score(moves[j], prev_p, prev_m.get_tgt());
            }
        }

        if let Some((prev_p, prev_m, _)) = self.get_previous_entry(2) {
            for j in 0..moves.len() {
                scores[j] += self.followup_moves.get_score(moves[j], prev_p, prev_m.get_tgt());
            }
        }
    }

    /// Get the stack entry from 'rollback' ply ago
    fn get_previous_entry(&self, rollback: usize) -> Option<(Piece, Move, usize)> {
        if self.ply >= rollback && self.search_stack[self.ply - rollback].1 != NULL_MOVE {
            Some(self.search_stack[self.ply - rollback])
        } else {
            None
        }
    }
}

/// Print UCI info
impl SearchInfo<'_> {
    /// Print pv by traversing the tt from the root
    fn print_pv(&self, mut board: Board, depth: usize) {
        for _ in 0..depth {
            let tt_move = match self.tt.probe(board.hash) {
                Some(e) => e.get_move(),
                None => break,
            };

            // move "sanity" check, since a hash collision is possible
            let move_list = board.gen_moves::<true>();

            if let Some(m) = tt_move {
                if move_list.moves.contains(&m) {
                    board = board.make_move(m);
                    print!(" {m}");
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        println!();
    }

    /// Print UCI score info
    pub fn print(&self, board: Board, eval: Eval, depth: usize) {
        let score = if eval.abs() >= MATE_IN_PLY {
            let moves_to_mate = (MATE - eval.abs() + 1) / 2;
            if eval > 0 {
                format!("mate {} ", moves_to_mate)
            } else {
                format!("mate -{} ", moves_to_mate)
            }
        } else {
            format!("cp {eval} ")
        };

        let time = self.clock.elapsed().as_millis().max(1);

        print!(
            "info time {} score {} depth {} seldepth {} nodes {} nps {} pv",
            time,
            score,
            depth,
            self.seldepth,
            self.nodes,
            (self.nodes as u128 * 1000) / time,
        );

        self.print_pv(board, depth);
    }
}
