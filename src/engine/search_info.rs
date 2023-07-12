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

        // Old counter becomes new followup, and the new move becomes the new counter
        self.followup_moves.history_move = self.counter_moves.history_move;
        self.counter_moves.history_move = Some((piece, m.get_tgt()));

        self.ply += 1;
        self.ply_from_null += 1;
        self.nodes += 1;
    }

    /// Push a null move to the search stack
    pub fn push_null(&mut self) {
        self.search_stack[self.ply] = (Piece::WP, NULL_MOVE, self.ply_from_null);

        // Null move only invalidates the counter move
        self.followup_moves.history_move = self.counter_moves.history_move;
        self.counter_moves.history_move = None;

        self.ply += 1;
        self.ply_from_null = 0;
        self.nodes += 1;
    }

    /// Pop a move from the search stack
    pub fn pop_move(&mut self) {
        self.ply -= 1;
        self.ply_from_null = self.search_stack[self.ply].2;

        // Fetch counter move from stack[ply - 1]
        if self.ply > 0 && self.search_stack[self.ply - 1].1 != NULL_MOVE {
            let (p, m, _) = self.search_stack[self.ply - 1];
            self.counter_moves.history_move = Some((p, m.get_tgt()));
        } else {
            self.counter_moves.history_move = None;
        }

        // Fetch followup move from stack[ply - 2]
        if self.ply > 1 && self.search_stack[self.ply - 2].1 != NULL_MOVE {
            let (p, m, _) = self.search_stack[self.ply - 2];
            self.followup_moves.history_move = Some((p, m.get_tgt()));
        } else {
            self.followup_moves.history_move = None;
        }
    }

    /// Upon a fail-high, update killer and history tables.
    pub fn update_tables(&mut self, m: Move, depth: usize, side: Color, searched: Vec<Move>) {
        let first_killer = self.killer_moves[self.ply][0];

        if first_killer != m {
            self.killer_moves[self.ply][1] = first_killer;
            self.killer_moves[self.ply][0] = m;
        }

        // Leaves can introduce a lot of random noise to history scores, don't consider them
        if depth < HISTORY_LOWER_LIMIT {
            return;
        }

        // Score histories
        let bonus = history_bonus(depth);

        self.history.update(bonus, m, side, &searched);
        self.counter_moves.update(bonus, m, &searched);
        self.followup_moves.update(bonus, m, &searched);
    }

    /// Return the full history score for a move made by the given side
    pub fn score_history(&self, m: Move, side: Color) -> i32 {
        self.history.get_score(m, side)
            + self.counter_moves.get_score(m)
            + self.followup_moves.get_score(m)
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
