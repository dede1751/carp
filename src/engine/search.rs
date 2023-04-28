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
            eval = self.alphabeta(&mut info, alpha, beta, depth);

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
    fn alphabeta(
        &mut self,
        info: &mut SearchInfo,
        mut alpha: Eval,
        beta: Eval,
        mut depth: usize,
    ) -> Eval {
        let root_node = info.ply == 0;
        let pv_node = alpha != beta - 1;
        let in_check = self.king_in_check();

        if !root_node && (info.stop || !info.clock.mid_check()) {
            info.stop = true;
            return 0;
        }

        if depth == 0 && !in_check {
            return self.quiescence(info, alpha, beta);
        }

        if !root_node {
            // Stop searching if the position is a rule-based draw
            // Add variance to draws to help with 3-fold (taken from Ethereal)
            if self.is_draw(info.ply_from_null) {
                return ((info.nodes & 3) - 2) as Eval;
            }

            // Mate distance pruning (it's a bit faster to do this before draw detection)
            // Prune if being mated now is too good or mating on the next ply too bad.
            let mate_alpha = alpha.max(-MATE + info.ply as Eval);
            let mate_beta = beta.min(MATE - info.ply as Eval - 1);
            if mate_alpha >= mate_beta {
                return mate_alpha;
            }
        }

        // Probe tt for eval and best move (for pv, only cutoff at leaves)
        let tt_hit;
        let stand_pat = match info.tt.probe(self.board.hash) {
            Some(entry) => {
                let tt_eval = entry.get_value(info.ply);
                let tt_flag = entry.get_flag();

                if entry.get_depth() >= depth
                    && (!pv_node || depth == 0)
                    && (tt_flag == TTFlag::Exact
                        || (tt_flag == TTFlag::Lower && tt_eval >= beta)
                        || (tt_flag == TTFlag::Upper && tt_eval <= alpha))
                {
                    return tt_eval;
                };

                info.sorter.tt_move = match tt_flag {
                    TTFlag::None => None,
                    _ => Some(entry.get_move()),
                };
                tt_hit = true;
                tt_eval
            }

            None => {
                info.sorter.tt_move = None;
                tt_hit = false;

                if in_check {
                    -INFINITY
                } else {
                    self.nnue_state.evaluate(self.board.side)
                }
            }
        };

        // When not in check, toss the stand pat back into the tt if it won't overwrite entries.
        if !tt_hit && !in_check {
            info.tt.insert(TTField::new(
                self,
                TTFlag::None,
                NULL_MOVE,
                stand_pat,
                depth,
                info.ply,
            ));
        }

        // Static pruning techniques:
        // these heuristics are trying to prove that the position is statically good enough to not
        // need any further deep search.
        if !pv_node && !in_check {
            // Reverse Futility Pruning (static eval pruning)
            // At pre-frontier nodes, check if the static eval minus a safety margin is enough to
            // produce a beta cutoff.
            if depth <= RFP_THRESHOLD && stand_pat - RFP_MARGIN * (depth as Eval) > beta {
                return stand_pat;
            }

            // Null Move Pruning (reduction value from CounterGO)
            // Give the opponent a "free shot" and see if that improves beta.
            if info.ply_from_null > 0
                && depth >= NMP_LOWER_LIMIT
                && stand_pat >= beta
                && !self.only_king_pawns_left()
            {
                let r = (NMP_BASE_R + depth / NMP_FACTOR).min(depth);

                self.make_null(info);
                let eval = -self.alphabeta(info, -beta, -beta + 1, depth - r);
                self.undo_move(info);

                // cutoff above beta and don't return fake mates
                if eval >= beta {
                    if eval >= MATE_IN_PLY {
                        return beta;
                    } else {
                        return eval;
                    }
                }
            }
        }

        // Internal Iterative Reductions
        // Replacement for IIR where "fresh" PV nodes without a tt entry get searched at a shallower
        // depth when high up in the tree.
        if !tt_hit && pv_node && depth >= IIR_LOWER_LIMIT {
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

        // Check Extension
        if in_check {
            depth += 1;
        }

        let old_alpha = alpha;
        let mut best_eval = -INFINITY;
        let mut best_move = NULL_MOVE;

        let mut skip_quiets = false;
        let mut searched_quiets = Vec::with_capacity(20);

        for (move_count, (m, s)) in move_list.enumerate() {
            if s < 0 && skip_quiets {
                break;
            }

            let is_quiet = !m.is_capture() && !m.is_promotion();

            // Quiet move pruning
            if !pv_node && !in_check && is_quiet && best_eval > -MATE_IN_PLY {
                let lmr_depth = depth - lmr_reduction(depth, move_count).min(depth);

                // History leaf pruning
                // Below a certain depth, prune negative history moves in non-pv nodes
                if lmr_depth <= HLP_THRESHOLD && s - HISTORY_OFFSET < HLP_MARGIN {
                    skip_quiets = true;
                }

                // Extended Futility pruning
                // Below a certain depth, prune moves which will most likely not improve alpha
                let efp_margin = EFP_BASE + EFP_MARGIN * (lmr_depth as Eval);
                if lmr_depth <= EFP_THRESHOLD && stand_pat + efp_margin < alpha {
                    skip_quiets = true;
                }

                // Late move pruning
                // Below a certain depth, start skipping later quiet moves
                if lmr_depth <= LMP_THRESHOLD && move_count >= LMP_BASE + (lmr_depth * lmr_depth) {
                    skip_quiets = true;
                }
            }

            // Principal Variation Search + Late Move Reductions
            // Before most searches, we run a "verification" search on a null window to prove it
            // fails high on alpha. If it doesn't, it's likely a cutnode.
            // We reduce the depth of these searches the further in the move list we go.
            self.make_move(m, info);
            let moves_made = move_count + 1;
            let mut eval = -INFINITY;

            let full_depth_search =
                if depth >= LMR_LOWER_LIMIT && moves_made >= LMR_THRESHOLD + pv_node as usize {
                    let r = if is_quiet || s < GOOD_CAPTURE {
                        let mut r = lmr_reduction(depth, moves_made) as i32;

                        r += !pv_node as i32; // reduce more in non-pv nodes
                        r -= (s > 0) as i32; // reduce less for killers/bad captures

                        r.clamp(1, (depth - 1) as i32) as usize
                    } else {
                        1
                    };

                    eval = -self.alphabeta(info, -alpha - 1, -alpha, depth - r);
                    eval > alpha && r > 1
                } else {
                    !pv_node || move_count > 0
                };

            // Full depth null window search when the first fails
            if full_depth_search {
                eval = -self.alphabeta(info, -alpha - 1, -alpha, depth - 1);
            }

            // Full depth full window search for the first move of all PV nodes
            if pv_node && (move_count == 0 || eval > alpha) {
                eval = -self.alphabeta(info, -beta, -alpha, depth - 1);
            }

            self.undo_move(info);

            if info.stop {
                return 0;
            }

            if eval > best_eval {
                best_eval = eval;
                best_move = m;
                
                if eval > alpha {
                    alpha = eval;

                    if eval >= beta {
                        if is_quiet {
                            info.update_sorter(m, depth, self.board.side, searched_quiets);
                        }
                        break;
                    }
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

            info.tt.insert(TTField::new(
                self, tt_flag, best_move, best_eval, depth, info.ply,
            ));
        }

        if root_node {
            info.best_move = best_move;
        }

        best_eval
    }

    fn quiescence(&mut self, info: &mut SearchInfo, mut alpha: Eval, beta: Eval) -> Eval {
        if info.stop || !info.clock.mid_check() {
            info.stop = true;
            return 0;
        }

        info.seldepth = info.ply.max(info.seldepth);

        if self.is_draw(info.ply_from_null) {
            return ((info.nodes & 3) - 2) as Eval;
        }

        // Stand pat and delta pruning
        let stand_pat = self.nnue_state.evaluate(self.board.side);
        if info.ply >= MAX_DEPTH || stand_pat >= beta || stand_pat < alpha - QS_DELTA_MARGIN {
            return stand_pat;
        }

        let in_check = self.king_in_check();
        let mut best_eval = stand_pat;
        alpha = alpha.max(stand_pat);

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

            if eval > best_eval {
                best_eval = eval;
                if eval > alpha {
                    if eval >= beta {
                        break;
                    }
                    alpha = eval;
                }
            }
        }

        best_eval
    }
}

impl<'a> SearchInfo<'a> {
    /// Update the sorter in case of a beta cutoff
    fn update_sorter(&mut self, m: Move, depth: usize, side: Color, searched_quiets: Vec<Move>) {
        let counter_move = if self.ply_from_null > 0 {
            Some(self.search_stack[self.ply - 1].0)
        } else {
            None
        };

        let followup_move = if self.ply_from_null > 1 {
            Some(self.search_stack[self.ply - 2].0)
        } else {
            None
        };

        self.sorter.update(
            m,
            counter_move,
            followup_move,
            self.ply,
            depth,
            side,
            searched_quiets,
        );
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
