/// Implement structures to have parallel search.
///
/// Carp uses the LazySMP parallelization scheme, which runs independent threads sharing only the TT
/// in lockless fashion to minimize synchronization overhead. In this scheme, a searching thread pool
/// has one main searching thread which also handles time controls, and multiple worker threads which
/// only stop when the main thread modifies the global stop flag.
use std::iter;
use std::sync::{
    atomic::{AtomicBool, AtomicU64, Ordering},
    Arc,
};
use std::thread;

use crate::{
    clock::{Clock, TimeControl},
    position::Position,
    search_params::*,
    search_tables::{history_bonus, ContinuationHistoryTable, HistoryTable, PVTable},
    syzygy::probe::{TB, TB_HITS},
    tt::TT,
};
use chess::{
    board::QUIETS,
    moves::Move,
    piece::{Color, Piece},
};

/// Information only relevant within the search tree (thread local)
#[derive(Clone, Debug)]
pub struct Thread {
    // Structures used by the search
    pub clock: Clock,
    search_stack: [(Piece, Move, usize); MAX_DEPTH],
    pub eval_stack: [Eval; MAX_DEPTH],
    pub excluded: [Option<Move>; MAX_DEPTH],

    // Move ordering
    pub killer_moves: [[Move; 2]; MAX_DEPTH],
    history: HistoryTable<HIST_MAX>,
    counter_moves: ContinuationHistoryTable<CONT_HIST_MAX>,
    followup_moves: ContinuationHistoryTable<CONT_HIST_MAX>,

    // Search stats
    pub nodes: u64,
    pub seldepth: usize,
    pub ply: usize,
    pub ply_from_null: usize,
    move_count: usize,

    // End of search
    pub pv: PVTable,
    pub eval: Eval,
    pub depth: usize,
    pub stop: bool,
}

/// Display UCI info
impl std::fmt::Display for Thread {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let score = if self.eval.abs() >= LONGEST_MATE {
            let moves_to_mate = (MATE - self.eval.abs() + 1) / 2;

            if self.eval > 0 {
                format!("mate {} wdl 1000 0 0", moves_to_mate)
            } else {
                format!("mate -{} wdl 0 0 1000", moves_to_mate)
            }
        } else {
            let norm_eval = self.normalize_cp_eval();
            let (w, d, l) = self.wdl_model(norm_eval);

            format!("cp {norm_eval} wdl {w} {d} {l}")
        };

        let time = self.clock.elapsed().as_millis().max(1);
        let nodes = self.clock.global_nodes();

        write!(
            f,
            "info time {} score {} depth {} seldepth {} nodes {} nps {} tbhits {} {}",
            time,
            score,
            self.depth,
            self.seldepth,
            nodes,
            (nodes as u128 * 1000) / time,
            TB_HITS.load(Ordering::SeqCst),
            self.pv
        )
    }
}

/// Implement WDL model for evaluation -- https://github.com/vondele/WLD_model
/// Normalizes a +100 cp advantage to a 50% chance of winning.
impl Thread {
    /// Normalize an evaluation score to a centipawn score (since nnue values are usually inflated)
    pub fn normalize_cp_eval(&self) -> Eval {
        const NORMALIZE_PAWN_VALUE: Eval = 199;

        if self.eval.abs() >= LONGEST_TB_MATE {
            self.eval
        } else {
            (self.eval * 100) / NORMALIZE_PAWN_VALUE
        }
    }

    /// Extract WDL scores from the (normalized) evaluation using a model fitted to self-play.
    pub fn wdl_model(&self, norm_eval: Eval) -> (Eval, Eval, Eval) {
        const AS: [f64; 4] = [-0.77690016, 10.19729841, 14.69567024, 175.35727553];
        const BS: [f64; 4] = [-3.74786075, 28.20402419, -53.21735403, 85.17319775];

        let phase = (self.move_count as f64).min(240.0) / 64.0;
        let a = (((AS[0] * phase + AS[1]) * phase + AS[2]) * phase) + AS[3];
        let b = (((BS[0] * phase + BS[1]) * phase + BS[2]) * phase) + BS[3];
        let score = (norm_eval as f64).clamp(-4000.0, 4000.0);

        let win_rate = (1000.0 / (1.0 + f64::exp((a - score) / b))) as i32;
        let loss_rate = (1000.0 / (1.0 + f64::exp((a + score) / b))) as i32;

        (win_rate, 1000 - win_rate - loss_rate, loss_rate)
    }
}

impl Thread {
    /// Create a new Thread struct with the given Clock.
    /// All other fields are initialized as empty.
    pub fn new(clock: Clock) -> Self {
        Self {
            clock,
            search_stack: [(Piece::WP, Move::NULL, 0); MAX_DEPTH],
            eval_stack: [0; MAX_DEPTH],
            excluded: [None; MAX_DEPTH],

            killer_moves: [[Move::NULL; 2]; MAX_DEPTH],
            history: HistoryTable::default(),
            counter_moves: ContinuationHistoryTable::default(),
            followup_moves: ContinuationHistoryTable::default(),

            nodes: 0,
            seldepth: 0,
            ply: 0,
            ply_from_null: 0,
            move_count: 0, // Init to -1 because we always increment first.

            pv: PVTable::default(),
            eval: -INFINITY,
            depth: 0,
            stop: false,
        }
    }

    /// Initialize a spinner thread with the given shared counters.
    /// Use this as either a placeholder thread to then set TC, or as a SMP worker thread.
    pub fn spinner(global_stop: Arc<AtomicBool>, global_nodes: Arc<AtomicU64>) -> Self {
        Self::new(Clock::spin_clock(global_stop, global_nodes))
    }

    /// Initialize a thread searching at a fixed depth.
    pub fn fixed_depth(depth: usize) -> Self {
        Self::new(Clock::new(
            Arc::new(AtomicBool::new(false)),
            Arc::new(AtomicU64::new(0)),
            TimeControl::FixedDepth(depth),
            false,
        ))
    }

    /// Get the current best move for the searching thread.
    pub fn best_move(&self) -> Move {
        self.pv.moves[0]
    }

    /// Advance a thread by the given amount of ply, resetting previous results.
    pub fn advance_ply(&mut self, advance: usize, ply: usize, halfmoves: usize) {
        // Killers are shifted back by the ply advance
        self.killer_moves.copy_within(advance.., 0);
        for k in &mut self.killer_moves[MAX_DEPTH - advance..] {
            *k = [Move::NULL; 2];
        }

        self.nodes = 0;
        self.clock.last_nodes = 0; // reset SMP worker threads
        self.seldepth = 0;
        self.ply = 0;
        self.ply_from_null = halfmoves;
        self.move_count = ply;

        self.pv = PVTable::default();
        self.eval = -INFINITY;
        self.depth = 0;
        self.stop = false;
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
        self.search_stack[self.ply] = (Piece::WP, Move::NULL, self.ply_from_null);
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
            self.counter_moves
                .update(bonus, best, p, m.get_tgt(), &searched);
        }

        if let Some((p, m, _)) = self.get_previous_entry(2) {
            self.followup_moves
                .update(bonus, best, p, m.get_tgt(), &searched);
        }
    }

    /// Assign history scores to movelist slices
    pub fn assign_history_scores(&self, side: Color, moves: &[Move], scores: &mut [i32]) {
        for i in 0..moves.len() {
            scores[i] = self.history.get_score(moves[i], side);
        }

        if let Some((prev_p, prev_m, _)) = self.get_previous_entry(1) {
            for j in 0..moves.len() {
                scores[j] += self
                    .counter_moves
                    .get_score(moves[j], prev_p, prev_m.get_tgt());
            }
        }

        if let Some((prev_p, prev_m, _)) = self.get_previous_entry(2) {
            for j in 0..moves.len() {
                scores[j] += self
                    .followup_moves
                    .get_score(moves[j], prev_p, prev_m.get_tgt());
            }
        }
    }

    /// Get the stack entry from 'rollback' ply ago
    fn get_previous_entry(&self, rollback: usize) -> Option<(Piece, Move, usize)> {
        if self.ply >= rollback && self.search_stack[self.ply - rollback].1 != Move::NULL {
            Some(self.search_stack[self.ply - rollback])
        } else {
            None
        }
    }
}

/// ThreadPool specific for handling LazySMP
#[derive(Clone, Debug)]
pub struct ThreadPool {
    main_thread: Thread,
    workers: Vec<Thread>,
    global_stop: Arc<AtomicBool>,
    global_nodes: Arc<AtomicU64>,
}

impl ThreadPool {
    /// Initialize a new threadpool with one single worker, holding the given global stop flag.
    pub fn new(global_stop: Arc<AtomicBool>) -> Self {
        let global_nodes = Arc::new(AtomicU64::new(0));

        Self {
            main_thread: Thread::spinner(global_stop.clone(), global_nodes.clone()),
            workers: Vec::new(),
            global_stop,
            global_nodes,
        }
    }

    /// Resize the threadpool to the given size, reinitializing all threads.
    pub fn resize(&mut self, workers: usize) {
        self.main_thread = Thread::spinner(self.global_stop.clone(), self.global_nodes.clone());
        self.workers.resize_with(workers, || {
            Thread::spinner(self.global_stop.clone(), self.global_nodes.clone())
        });
    }

    /// Reset a threadpool to prepare for the start of the game.
    pub fn reset(&mut self) {
        self.resize(self.workers.len());
    }

    /// Deploy a parallel search using LazySMP, returning the agreed-upon best move.
    pub fn deploy_search(
        &mut self,
        pos: &mut Position,
        tt: &TT,
        tb: TB,
        time_control: TimeControl,
    ) -> Move {
        // Setup all threads to start the search.
        self.main_thread.clock = Clock::new(
            self.global_stop.clone(),
            self.global_nodes.clone(),
            time_control,
            pos.white_to_move(),
        );
        self.main_thread
            .advance_ply(2, pos.ply(), pos.board.halfmoves);
        self.workers
            .iter_mut()
            .for_each(|t| t.advance_ply(2, pos.ply(), pos.board.halfmoves));

        self.global_stop.store(false, Ordering::SeqCst);
        self.global_nodes.store(0, Ordering::SeqCst);

        // If we're in a TB position and get a result, return early.
        if let Some(result) = tb.probe_root(&pos.board) {
            self.main_thread.pv = PVTable::default();
            self.main_thread
                .pv
                .update_pv_line(result.best_move, &PVTable::default());
            self.main_thread.eval = result.wdl.to_eval(0);

            TB_HITS.store(1, Ordering::SeqCst);
            println!("{}", self.main_thread);

            return result.best_move;
        } else {
            TB_HITS.store(0, Ordering::SeqCst);
        }

        // Return immediately in forced situations.
        let move_list = pos.board.gen_moves::<QUIETS>();
        let move_count = move_list.len();

        if move_count == 0 {
            return Move::NULL;
        } else if move_count == 1 || self.main_thread.clock.no_search_time() {
            return move_list.moves[0];
        };

        thread::scope(|scope| {
            let mut worker_handles = Vec::with_capacity(self.workers.len());

            // Deploy all worker search threads.
            for t in self.workers.iter_mut() {
                let mut worker_pos = pos.clone();
                worker_handles
                    .push(scope.spawn(move || worker_pos.iterative_search::<false>(t, tt, tb)));
            }

            // Run the main search thread with info enabled
            // Explicitly stop all other workers in case we exceded the depth limit.
            pos.iterative_search::<true>(&mut self.main_thread, tt, tb);
            self.global_stop.store(true, Ordering::SeqCst);
        });

        // Take the moves at highest depth, and from those the ones which occur the most
        let results = iter::once(&self.main_thread).chain(self.workers.iter());
        let highest_depth = results.clone().max_by_key(|t| t.depth).unwrap().depth;

        results
            .filter_map(|t| {
                if t.depth == highest_depth {
                    Some(t.best_move())
                } else {
                    None
                }
            })
            .fold(
                std::collections::HashMap::<Move, u8>::new(),
                |mut map, x| {
                    *map.entry(x).or_default() += 1;
                    map
                },
            )
            .into_iter()
            .max_by_key(|(_, value)| *value)
            .unwrap()
            .0 // always at least one search, impossible panic
    }
}
