use std::thread;

use crate::chess::{board::*, moves::*, tables::*};
use crate::engine::{clock::*, move_sorter::*, position::*, search_params::*, tt::*};

/// Information only relevant within the search tree
pub struct SearchInfo<'a> {
    pub clock: Clock,
    pub tt: &'a TT,
    pub sorter: MoveSorter,
    pub nodes: u64,
    pub seldepth: usize,
    pub ply: usize,
    pub ply_from_null: usize,

    pub best_move: Move,
    pub search_stack: [(Move, usize); MAX_DEPTH],
    pub stop: bool,
}

pub struct SearchResult {
    pub best_move: Move,
    pub eval: Eval,
    pub depth: usize,
    pub nodes: u64,
}

/// Implement tree search functions for a position
impl Position {
    /// Deploy a multitreaded search with given number of workers using Lazy SMP
    /// Returns the move at the highest depth which occurs the most
    pub fn smp_search(&mut self, worker_count: usize, main_clock: Clock, tt: &TT) -> Move {
        let move_list = self.board.gen_moves::<true>();
        let move_count = move_list.len();

        // Return immediately in forced situations.
        if move_count == 0 {
            return NULL_MOVE;
        } else if move_count == 1 {
            return move_list.moves[0];
        };

        thread::scope(|scope| {
            let mut worker_handles = Vec::with_capacity(worker_count);
            let mut results = Vec::with_capacity(worker_count + 1);

            // Deploy all worker search threads.
            for _ in 1..worker_count {
                let mut worker_pos = self.clone();
                let worker_clock = main_clock.get_child_clock();

                worker_handles.push(
                    scope.spawn(move || worker_pos.iterative_search::<false>(worker_clock, tt)),
                );
            }

            // Run the main search in this thread (with info enabled)
            results.push(self.iterative_search::<true>(main_clock, tt));

            for handle in worker_handles {
                match handle.join() {
                    Ok(result) => results.push(result),
                    Err(e) => eprintln!("{e:?}"),
                }
            }

            // Take the moves at highest depth, and from those the ones which occur the most
            let highest_depth = results.iter().max_by_key(|i| i.depth).unwrap().depth;

            results
                .into_iter()
                .filter_map(|i| {
                    if i.depth == highest_depth {
                        Some(i.best_move)
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
        })
    }

    /// Iteratively searches the position at increasing depth
    /// After the shallower depths, we start doing reduced-window searches and eventually reopen
    /// each "side" of the window in case of fail-high or fail-low
    pub fn iterative_search<const INFO: bool>(&mut self, clock: Clock, tt: &TT) -> SearchResult {
        let mut info = SearchInfo {
            clock,
            tt,
            sorter: MoveSorter::default(),
            nodes: 0,
            seldepth: 0,
            ply: 0,
            ply_from_null: 0,
            best_move: NULL_MOVE,
            search_stack: [(NULL_MOVE, 0); MAX_DEPTH],
            stop: false,
        };

        let mut result = SearchResult {
            best_move: NULL_MOVE,
            eval: -INFINITY,
            depth: 0,
            nodes: 0,
        };

        let mut depth = 1;
        let mut alpha = -INFINITY;
        let mut beta = INFINITY;
        let mut eval;

        while !info.stop && info.clock.start_check(depth, info.nodes) && depth < MAX_DEPTH {
            eval = self.negamax(&mut info, alpha, beta, depth);

            if eval <= alpha {
                alpha = -INFINITY;
            } else if eval >= beta {
                beta = INFINITY;
            } else {
                if depth >= ASPIRATION_THRESHOLD {
                    alpha = eval - ASPIRATION_WINDOW;
                    beta = eval + ASPIRATION_WINDOW;
                }

                if !info.stop {
                    if INFO {
                        info.print(self.board, eval, depth);
                        info.seldepth = 0;
                    }
                    result.best_move = info.best_move;
                    result.eval = eval;
                    result.depth = depth;
                    depth += 1;
                }
            }
        }

        result.nodes = info.nodes;
        result
    }

    /// Standard alpha-beta negamax tree search
    fn negamax(
        &mut self,
        info: &mut SearchInfo,
        mut alpha: Eval,
        mut beta: Eval,
        mut depth: usize,
    ) -> Eval {
        if info.stop || !info.clock.mid_check() {
            info.stop = true;
            return 0;
        }

        let root_node = info.ply == 0;
        let pv_node = alpha != beta - 1; // False when searching with a null window
        let in_check = self.king_in_check();

        // Check extension
        if in_check {
            depth += 1;
        }

        if depth == 0 {
            return self.quiescence(info, alpha, beta);
        }

        // Mate distance pruning (it's a bit faster to do this before draw detection)
        if !root_node {
            alpha = alpha.max(-MATE + info.ply as Eval);
            beta = beta.min(MATE - info.ply as Eval - 1);
            if alpha >= beta {
                return alpha;
            }
    
            // Stop searching if the position is a rule-based draw
            if self.is_draw(info.ply_from_null) {
                return 0;
            }
        }

        // Probe tt for eval and best move (for pv, only cutoff at leaves)
        match info.tt.probe(self.board.hash) {
            Some(entry) => {
                let tt_move = entry.get_move();

                if pv_node {
                    if !root_node && depth == 1 && entry.get_flag() == TTFlag::Exact {
                        return entry.get_eval(info.ply);
                    }
                } else if entry.get_depth() >= depth {
                    let tt_eval = entry.get_eval(info.ply);

                    match entry.get_flag() {
                        TTFlag::Exact => return tt_eval,
                        TTFlag::Upper => beta = beta.min(tt_eval),
                        TTFlag::Lower => alpha = alpha.max(tt_eval),
                    }

                    // Upper/Lower flags can cause indirect cutoffs!
                    if alpha >= beta {
                        return tt_eval;
                    }
                }
                info.sorter.tt_move = Some(tt_move);
            }

            None => info.sorter.tt_move = None,
        };
        let tt_hit = info.sorter.tt_move.is_some();

        // Static pruning techniques:
        // these heuristics are trying to prove that the position is statically good enough to not
        // need any further deep search.
        let mut stand_pat = 0;

        if !pv_node && !in_check {
            // Reverse Futility Pruning (static eval pruning)
            // At pre-frontier nodes, check if the static eval minus a safety margin is enough to
            // produce a beta cutoff.
            if depth <= RFP_THRESHOLD {
                stand_pat = self.nnue_state.evaluate(self.board.side); // this remains valid for efp

                if stand_pat - RFP_MARGIN * (depth as Eval) >= beta {
                    return beta;
                }
            }

            // Null Move Pruning (reduction value from CounterGO)
            // Give the opponent a "free shot" and see if that improves beta.
            if depth > NMP_LOWER_LIMIT && !self.only_king_pawns_left() {
                let reduction = NMP_BASE_R + depth / NMP_FACTOR;

                self.make_null(info);
                let eval = -self.negamax(info, -beta, -beta + 1, depth.saturating_sub(reduction));
                self.undo_move(info);

                // cutoff above beta and not for mate scores!
                if eval >= beta {
                    return beta;
                }
            }
        }

        // Internal Iterative Reduction
        // When no TT Move is found in any node, apply a 1-ply reduction
        if !root_node && !tt_hit && depth >= IIR_LOWER_LIMIT {
            depth -= 1;
        }

        let move_list = self.generate_moves(info.ply, &info.sorter);

        // Mate or stalemate. Don't save in the TT, simply return early
        if move_list.is_empty() {
            if in_check {
                return -MATE + info.ply as Eval;
            } else {
                return 0;
            }
        };

        let mut eval: Eval;
        let mut searched_quiets = Vec::with_capacity(20);
        let mut tt_field = TTField::new(self, TTFlag::Upper, NULL_MOVE, alpha, depth, info.ply);

        for (move_count, (m, s)) in move_list.enumerate() {
            self.make_move(m, info);

            // Flag for moves checking the opponent
            let is_check = self.king_in_check();
            let is_quiet = !m.is_capture() && !m.is_promotion();

            // Quiet move pruning
            if !pv_node && !in_check && is_quiet && !is_check && alpha >= -MATE_IN_PLY {
                let mut prune = false;

                // History leaf pruning
                // Below a certain depth, prune negative history moves in non-pv nodes
                if depth <= HLP_THRESHOLD && s - HISTORY_OFFSET < HLP_MARGIN {
                    prune = true;
                }

                // Extended Futility pruning
                // Below a certain depth, prune moves which will most likely not improve alpha
                if !prune
                    && depth <= EFP_THRESHOLD
                    && stand_pat + EFP_BASE + EFP_MARGIN * (depth as Eval) < alpha
                {
                    prune = true;
                }

                // Late move pruning
                if !prune && depth <= LMP_THRESHOLD && move_count >= LMP_BASE + (depth * depth) {
                    prune = true;
                }

                // even when all moves get pruned, save something to the tt
                if prune {
                    self.undo_move(info);

                    if move_count == 0 {
                        tt_field.update_data(TTFlag::Upper, m, alpha);
                    }
                    break;
                }
            }

            if move_count == 0 {
                // full depth search on first move
                eval = -self.negamax(info, -beta, -alpha, depth - 1);
                tt_field.update_data(TTFlag::Upper, m, alpha); // always save at least one move
            } else {
                // reduce depth for all moves beyond first
                let reduced_depth =
                    if !root_node && move_count >= LMR_THRESHOLD && depth >= LMR_LOWER_LIMIT && is_quiet {
                        let lmr_red = lmr_reduction(depth, move_count) as i32;
                        let lmr_ext = is_check as i32 + in_check as i32 + pv_node as i32;

                        (depth as i32 + lmr_ext - lmr_red).clamp(1, depth as i32) as usize
                    } else {
                        depth
                    };

                // do reduced depth pvs search, and eventually fall back to full window
                eval = -self.negamax(info, -alpha - 1, -alpha, reduced_depth - 1);
                if eval > alpha && pv_node && !info.stop {
                    eval = -self.negamax(info, -beta, -alpha, reduced_depth - 1);
                }

                // fall back to full depth if lmr failed
                if reduced_depth < depth && eval > alpha && !info.stop {
                    eval = -self.negamax(info, -alpha - 1, -alpha, depth - 1);
                    if eval > alpha && pv_node && !info.stop {
                        eval = -self.negamax(info, -beta, -alpha, depth - 1);
                    }
                }
            };

            self.undo_move(info);

            if info.stop {
                return 0;
            }

            if eval > alpha {
                if eval >= beta {
                    if is_quiet {
                        info.sorter.update(m, info.ply, depth, self.board.side, searched_quiets);
                    };

                    alpha = beta;
                    tt_field.update_data(TTFlag::Lower, m, beta);
                    break;
                }

                alpha = eval;
                tt_field.update_data(TTFlag::Exact, m, eval);
            }

            // save searched quiets that didn't cause a cutoff for negative history score
            if is_quiet {
                searched_quiets.push(m);
            }
        }

        if !info.stop {
            info.tt.insert(tt_field);
        }

        if root_node {
            info.best_move = tt_field.get_move();
        }

        alpha
    }

    fn quiescence(&mut self, info: &mut SearchInfo, mut alpha: Eval, beta: Eval) -> Eval {
        if info.stop || !info.clock.mid_check() {
            info.stop = true;
            return 0;
        }

        info.seldepth = info.seldepth.max(info.ply);

        // Stand pat and delta pruning
        let stand_pat = self.nnue_state.evaluate(self.board.side);

        if info.ply >= MAX_DEPTH {
            return stand_pat;
        }

        if stand_pat >= beta {
            return beta;
        }
        if stand_pat < alpha - QS_DELTA_MARGIN {
            return alpha;
        }
        alpha = alpha.max(stand_pat);

        let in_check = self.king_in_check();
        for (m, s) in self.generate_captures(&info.sorter) {
            if !in_check {
                if s < GOOD_CAPTURE {
                    break; // we reached negative see, it's probably not worth searching
                }

                // futility pruning
                let move_value = stand_pat + QS_PIECE_VALUES[m.get_capture() as usize / 2];
                if !m.is_promotion() && move_value + QS_FUTILITY_MARGIN < alpha {
                    continue;
                }
            }

            self.make_move(m, info);
            let eval = -self.quiescence(info, -beta, -alpha);
            self.undo_move(info);

            if info.stop {
                return 0;
            }

            if eval > alpha {
                if eval >= beta {
                    return beta;
                }
                alpha = eval;
            }
        }

        alpha
    }
}

impl<'a> SearchInfo<'a> {
    // Push a non-null move to the search stack
    pub fn push_move(&mut self, m: Move) {
        self.search_stack[self.ply] = (m, self.ply_from_null);
        self.sorter.followup_move = self.sorter.counter_move;
        self.sorter.counter_move = Some(m);

        self.ply += 1;
        self.ply_from_null += 1;
        self.nodes += 1;
    }

    // Push a null move to the search stack
    pub fn push_null(&mut self) {
        self.search_stack[self.ply] = (NULL_MOVE, self.ply_from_null);
        self.sorter.followup_move = None;
        self.sorter.counter_move = None;

        self.ply += 1;
        self.ply_from_null = 0;
        self.nodes += 1;
    }

    // Pop a move from the search stack
    pub fn pop_move(&mut self) {
        self.ply -= 1;
        let (_, old_ply) = self.search_stack[self.ply];

        self.ply_from_null = old_ply;
        self.sorter.counter_move = self.sorter.followup_move;
        if self.ply_from_null > 1 {
            self.sorter.followup_move = Some(self.search_stack[self.ply - 2].0);
        }
    }

    /// Print pv by traversing the tt from the root
    fn print_pv(&self, mut board: Board, depth: usize) {
        for _ in 0..depth {
            let tt_move = match self.tt.probe(board.hash) {
                Some(e) => e.get_move(),
                None => break,
            };

            // move "sanity" check, since a hash collision is possible
            let move_list = board.gen_moves::<true>();

            if move_list.moves.contains(&tt_move) {
                board = board.make_move(tt_move);
                print!(" {tt_move}");
            } else {
                break;
            }
        }
        println!();
    }

    /// Print UCI score info
    fn print(&self, board: Board, eval: Eval, depth: usize) {
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

/// Test nodes searched
/// Run with: cargo test --release search -- --show-output
#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::sync::{atomic::AtomicBool, Arc};
    use std::time::Instant;

    use crate::chess::piece::Color;

    fn search_driver(fen: &str, depth: usize) {
        init_all_tables();
        let mut position: Position = fen.parse().unwrap();
        let tt = TT::default();
        let clock = Clock::new(
            TimeControl::FixedDepth(depth),
            Arc::new(AtomicBool::new(false)),
            position.board.side == Color::White,
        );

        println!("\n{}\n\n", position.board);

        let start = Instant::now();
        let best_move = position.iterative_search::<true>(clock, &tt).best_move;
        let duration = start.elapsed();

        println!(
            "\nDEPTH: {depth} Found {best_move} in {duration:?}\n--------------------------------\n",
        );
    }

    #[test]
    fn search_kiwipete15() {
        search_driver(
            "fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            15,
        );
    }

    #[test]
    fn search_killer15() {
        search_driver(
            "fen rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1",
            15,
        );
    }

    #[test]
    fn search_mate4() {
        search_driver("fen 8/8/8/2K5/5Q2/8/4k3/8 w - - 0 1", 20);
    }
}
