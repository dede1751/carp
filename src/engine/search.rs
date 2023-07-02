use std::thread;

use crate::chess::{board::*, moves::*, piece::*, tables::*};
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
    pub search_stack: [(Move, Piece, usize); MAX_DEPTH],
    pub eval_stack: [Eval; MAX_DEPTH],
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
        } else if move_count == 1 || main_clock.no_search_time() {
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
            search_stack: [(NULL_MOVE, Piece::WP, 0); MAX_DEPTH],
            eval_stack: [0; MAX_DEPTH],
            stop: false,
        };

        let mut result = SearchResult {
            best_move: NULL_MOVE,
            eval: -INFINITY,
            depth: 0,
            nodes: 0,
        };

        for d in 1..MAX_DEPTH {
            if !info.clock.start_check(d, info.nodes, result.best_move) {
                break;
            }

            let eval = self.aspiration_window(&mut info, &mut result.best_move, result.eval, d);

            if !info.stop {
                result.best_move = info.best_move;
                result.eval = eval;
                result.depth = d;

                if INFO {
                    info.print(self.board, eval, d);
                }
            }
        }

        result.nodes = info.nodes;
        result
    }

    /// Aspiration Window loop
    /// Run searches on progressively wider windows until we find a value within the window.
    fn aspiration_window(
        &mut self,
        info: &mut SearchInfo,
        best_move: &mut Move,
        prev: Eval,
        mut depth: usize,
    ) -> Eval {
        let base_depth = depth;

        // Setup aspiration window when searching a sufficient depth
        let mut alpha = -INFINITY;
        let mut beta = INFINITY;
        let mut delta = ASPIRATION_WINDOW;
        if depth >= ASPIRATION_THRESHOLD {
            alpha = (-INFINITY).max(prev - delta);
            beta = (INFINITY).min(prev + delta);
        }

        loop {
            let eval = self.negamax(info, alpha, beta, depth);
            if info.stop {
                return 0;
            }

            if eval <= alpha {
                // Fail-low, reset search depth, widen window down
                beta = (alpha + beta) / 2;
                alpha = (-INFINITY).max(alpha - delta);
                depth = base_depth;
            } else if eval >= beta {
                // Fail-high, reduce depth, widen window up
                beta = (INFINITY).min(beta + delta);

                if eval.abs() < MATE_IN_PLY && depth > 1 {
                    depth -= 1;
                }

                // Short circuit saving the best move when failing high
                *best_move = info.best_move;
            } else {
                // Search within window, success
                return eval;
            }

            // Widen window, fully reopen when it's too wide
            delta += delta / 2;
            if delta >= QS_DELTA_MARGIN {
                alpha = -INFINITY;
                beta = INFINITY;
            }
        }
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

        if root_node {
            info.seldepth = 0;
        } else {
            info.seldepth = info.seldepth.max(info.ply);
        }

        // Check extension
        if in_check {
            depth += 1;
        }

        // Quiescence search
        if depth == 0 {
            return self.quiescence(info, alpha, beta);
        }

        if !root_node {
            // Mate distance pruning
            // Shrink the window based on the best/worst possible outcomes, which are being mated
            // now or mating in the next ply. Prune if even these extreme situations would not
            // produce a line better than the PV
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

        // Probe tt for the best move and static eval (for pv, only cutoff at leaves)
        let tt_entry = info.tt.probe(self.board.hash);
        let mut stand_pat = -INFINITY;
        let mut tt_move = None;

        if let Some(entry) = tt_entry {
            let tt_flag = entry.get_flag();
            let tt_eval = entry.get_eval(info.ply);

            // TT cutoffs
            if entry.get_depth() >= depth {
                if !root_node && pv_node && tt_flag == TTFlag::Exact && depth == 1 {
                    return tt_eval;
                } else if !pv_node {
                    match tt_flag {
                        TTFlag::Exact => return tt_eval,
                        TTFlag::Lower if tt_eval >= beta => return beta,
                        TTFlag::Upper if tt_eval <= alpha => return alpha,
                        _ => (),
                    }
                }
            }

            // If we're in check, we do not use the static eval.
            // We recompute the tt static eval when it's a bad value (-INFINITY)
            if !in_check {
                let tt_static_eval = entry.get_static_eval();

                if tt_static_eval == -INFINITY {
                    stand_pat = self.nnue_state.evaluate(self.board.side);
                } else {
                    stand_pat = tt_static_eval;
                }

                // If the tt eval is a tigher bound than the static eval, use it as stand pat
                if (tt_flag == TTFlag::Lower && tt_eval > tt_static_eval)
                    || (tt_flag == TTFlag::Upper && tt_eval <= tt_static_eval)
                {
                    stand_pat = tt_eval;
                }
            }

            tt_move = entry.get_move();
        } else if !in_check {
            // Without a tt entry (and not in check), we have to compute the static eval
            stand_pat = self.nnue_state.evaluate(self.board.side);

            // Chuck the static eval into the tt. This won't overwrite any relevant entry
            info.tt.insert(
                self.board.hash,
                TTFlag::None,
                NULL_MOVE,
                -INFINITY,
                stand_pat,
                0,
                info.ply,
            );
        }

        info.eval_stack[info.ply] = stand_pat;

        // Improving is true when the current static eval is better than that of a move ago
        // Assuming this trend continues down this branch, we can prune high more aggressively,
        // while we should be less aggressive when pruning low.
        let improving = !in_check && info.ply > 1 && stand_pat > info.eval_stack[info.ply - 2];

        // Static pruning techniques:
        // these heuristics are trying to prove that the position is statically good enough to not
        // need any further deep search.
        if !pv_node && !in_check {
            // Reverse Futility Pruning (static eval pruning)
            // At pre-frontier nodes, check if the static eval minus a safety margin is enough to
            // produce a beta cutoff.
            let rfp_margin =
                RFP_MARGIN * (depth as Eval) - RFP_IMPROVING_MARGIN * (improving as Eval);
            if depth <= RFP_THRESHOLD && stand_pat - rfp_margin >= beta {
                return beta;
            }

            // Null Move Pruning (reduction value from CounterGO)
            // Give the opponent a "free shot" and see if that improves beta.
            if depth > NMP_LOWER_LIMIT
                && stand_pat + NMP_IMPROVING_MARGIN * (improving as Eval) >= beta
                && info.ply_from_null > 0
                && !self.only_king_pawns_left()
            {
                let r = (NMP_BASE_R + depth / NMP_FACTOR).min(depth);

                self.make_null(info);
                let eval = -self.negamax(info, -beta, -beta + 1, depth - r);
                self.undo_move(info);

                // cutoff above beta
                if eval >= beta {
                    return beta;
                }
            }
        }

        // Internal Iterative Reduction
        // When no TT Move is found in any node, apply a 1-ply reduction
        if !root_node && tt_move.is_none() && depth >= IIR_LOWER_LIMIT {
            depth -= 1;
        }

        info.sorter.tt_move = tt_move;
        let move_list = self.generate_moves(info.ply, &info.sorter);

        // Mate or stalemate. Don't save in the TT, simply return early
        if move_list.is_empty() {
            if in_check {
                return -MATE + info.ply as Eval;
            } else {
                return 0;
            }
        };

        let old_alpha = alpha;
        let mut best_move = NULL_MOVE;
        let mut best_eval = -INFINITY;
        let mut searched_quiets = Vec::with_capacity(20);
        let lmp_count = LMP_BASE + (depth * depth);

        for (move_count, (m, s)) in move_list.enumerate() {
            let start_nodes = info.nodes;

            self.make_move(m, info);
            info.tt.prefetch(self.board.hash); // prefetch next hash

            // Flag for moves checking the opponent
            let is_check = self.king_in_check();
            let is_quiet = m.get_type().is_quiet();

            // Quiet move pruning
            if !pv_node && !in_check && is_quiet && !is_check && alpha >= -MATE_IN_PLY {
                let mut prune = false;

                // History leaf pruning
                // Below a certain depth, prune negative history moves in non-pv nodes
                if depth <= HLP_THRESHOLD && s - HISTORY_OFFSET < HLP_MARGIN {
                    prune = true;
                }

                let lmr_depth = depth - lmr_reduction(depth, move_count).min(depth);

                // Extended Futility pruning
                // Below a certain depth, prune moves which will most likely not improve alpha
                let efp_margin = EFP_BASE + EFP_MARGIN * (lmr_depth as Eval);
                if !prune && lmr_depth <= EFP_THRESHOLD && stand_pat + efp_margin < alpha {
                    prune = true;
                }

                // Late move pruning
                if !prune && depth <= LMP_THRESHOLD && move_count >= lmp_count {
                    prune = true;
                }

                // even when all moves get pruned, save something to the tt
                if prune {
                    self.undo_move(info);
                    break;
                }
            }

            // Principal Variation Search + Late Move Reductions
            // Before most searches, we run a "verification" search on a null window to prove it
            // fails high on alpha. If it doesn't, it's likely a cutnode.
            // We reduce the depth of these searches the further in the move list we go.
            let mut eval = -INFINITY;
            let full_depth_search =
                if depth >= LMR_LOWER_LIMIT && move_count >= LMR_THRESHOLD + pv_node as usize {
                    let r = if is_quiet {
                        let mut r = lmr_reduction(depth, move_count) as i32;

                        r += !pv_node as i32; // reduce more in non-pv nodes
                        r -= in_check as i32 + is_check as i32; // reduce less when in check/checking the opponent

                        r.clamp(1, (depth - 1) as i32) as usize
                    } else {
                        1
                    };

                    // Reduced depth null window search
                    eval = -self.negamax(info, -alpha - 1, -alpha, depth - r);
                    eval > alpha && r > 1
                } else {
                    !pv_node || move_count > 0
                };

            // Full depth null window search when lmr fails or when using pvs
            if full_depth_search {
                eval = -self.negamax(info, -alpha - 1, -alpha, depth - 1);
            }

            // Full depth full window search for the first move of all PV nodes and when pvs fails
            if pv_node && (move_count == 0 || eval > alpha) {
                eval = -self.negamax(info, -beta, -alpha, depth - 1);
            }

            self.undo_move(info);

            if info.stop {
                return 0;
            }

            // In root, update the node counts used by the clock
            if root_node {
                info.clock.update_node_counts(m, info.nodes - start_nodes);
            }

            if eval > best_eval {
                best_eval = eval;

                if eval > alpha {
                    best_move = m;
                    alpha = eval;
                }

                if eval >= beta {
                    if is_quiet {
                        info.sorter
                            .update(m, info.ply, depth, self.board.side, searched_quiets);
                    };

                    alpha = beta;
                    break;
                }
            }

            // save searched quiets that didn't cause a cutoff for negative history score
            if is_quiet {
                searched_quiets.push(m);
            }
        }

        if !info.stop {
            let tt_flag = if best_eval >= beta {
                TTFlag::Lower
            } else if best_eval > old_alpha {
                TTFlag::Exact
            } else {
                TTFlag::Upper
            };

            info.tt.insert(
                self.board.hash,
                tt_flag,
                best_move,
                alpha,
                stand_pat,
                depth,
                info.ply,
            );
        }

        if root_node {
            info.best_move = best_move;
        }

        alpha
    }

    /// Quiescence search: only search captures to avoid the horizon effect
    fn quiescence(&mut self, info: &mut SearchInfo, mut alpha: Eval, beta: Eval) -> Eval {
        if info.stop || !info.clock.mid_check() {
            info.stop = true;
            return 0;
        }

        info.seldepth = info.seldepth.max(info.ply);

        // Return early when reaching max depth
        if info.ply >= MAX_DEPTH {
            return self.nnue_state.evaluate(self.board.side);
        }

        let in_check = self.king_in_check();
        let old_alpha = alpha;
        let mut stand_pat = -INFINITY;

        // Probe tt and compute stand pat
        if let Some(entry) = info.tt.probe(self.board.hash) {
            let tt_eval = entry.get_eval(info.ply);
            let tt_flag = entry.get_flag();

            // TT Cutoffs
            match tt_flag {
                TTFlag::Exact => return tt_eval,
                TTFlag::Lower if tt_eval >= beta => return beta,
                TTFlag::Upper if tt_eval <= alpha => return alpha,
                _ => (),
            }

            // Use the static eval in TT when appropriate, else use NNUE
            if !in_check {
                let tt_static_eval = entry.get_static_eval();

                if tt_static_eval == -INFINITY {
                    stand_pat = self.nnue_state.evaluate(self.board.side);
                } else {
                    stand_pat = tt_static_eval;
                }

                // If the tt eval is a tigher bound than the static eval, use it as stand pat
                if (tt_flag == TTFlag::Lower && tt_eval > tt_static_eval)
                    || (tt_flag == TTFlag::Upper && tt_eval <= tt_static_eval)
                {
                    stand_pat = tt_eval;
                }
            }
        } else if !in_check {
            stand_pat = self.nnue_state.evaluate(self.board.side)
        };

        // Stand pat and delta pruning
        if !in_check {
            alpha = alpha.max(stand_pat);

            if stand_pat >= beta || stand_pat + QS_DELTA_MARGIN < alpha {
                return stand_pat;
            }
        }

        let mut best_move = NULL_MOVE;
        let mut best_eval = stand_pat;

        for (m, s) in self.generate_captures(&info.sorter) {
            if !in_check {
                // SEE pruning
                // Avoid searching captures with bad static evaluation
                if s < GOOD_CAPTURE {
                    break;
                }

                // Futility Pruning
                // Avoid searching captures that, even with an extra margin, would not raise alpha
                let move_value = stand_pat + PIECE_VALUES[self.board.get_capture(m) as usize];
                if !m.get_type().is_promotion() && move_value + QS_FUTILITY_MARGIN < alpha {
                    continue;
                }
            }

            self.make_move(m, info);
            info.tt.prefetch(self.board.hash); // prefetch next hash
            let eval = -self.quiescence(info, -beta, -alpha);
            self.undo_move(info);

            if info.stop {
                return 0;
            }

            if eval > best_eval {
                best_eval = eval;

                if eval > alpha {
                    best_move = m;
                    alpha = eval;
                }

                if eval >= beta {
                    alpha = beta;
                    break;
                }
            }
        }

        // Cosmo (Viridithas) trick: when in check and all moves are bad, return a "pseudo-mate" score
        if in_check && best_eval == -INFINITY {
            return -5000;
        }

        // Save to TT if we at least improved on the static eval.
        if !info.stop {
            let tt_flag = if best_eval >= beta {
                TTFlag::Lower
            } else if best_eval > old_alpha {
                TTFlag::Exact
            } else {
                TTFlag::Upper
            };

            info.tt.insert(
                self.board.hash,
                tt_flag,
                best_move,
                alpha,
                stand_pat,
                0,
                info.ply,
            );
        }

        alpha
    }
}

impl<'a> SearchInfo<'a> {
    /// Push a non-null move to the search stack
    pub fn push_move(&mut self, m: Move, piece: Piece) {
        self.search_stack[self.ply] = (m, piece, self.ply_from_null);
        self.sorter.followup = self.sorter.counter;
        self.sorter.counter = Some((m, piece));

        self.ply += 1;
        self.ply_from_null += 1;
        self.nodes += 1;
    }

    /// Push a null move to the search stack
    pub fn push_null(&mut self) {
        self.search_stack[self.ply] = (NULL_MOVE, Piece::WP, self.ply_from_null);
        self.sorter.followup = None;
        self.sorter.counter = None;

        self.ply += 1;
        self.ply_from_null = 0;
        self.nodes += 1;
    }

    /// Pop a move from the search stack
    pub fn pop_move(&mut self) {
        self.ply -= 1;
        let (_, _, old_ply) = self.search_stack[self.ply];

        self.ply_from_null = old_ply;
        self.sorter.counter = self.sorter.followup;
        if self.ply_from_null > 1 {
            let (m, p, _) = self.search_stack[self.ply - 2];
            self.sorter.followup = Some((m, p));
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
