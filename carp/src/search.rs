/// The Search module implements Carp's Alpha-Beta algorithm for single-threaded tree search.
use crate::{move_picker::*, position::*, search_params::*, search_tables::*, thread::*, tt::*};
use chess::{board::*, moves::*, tables::*};

impl Position {
    /// Iteratively searches the position at increasing depth
    /// Search results remain stored within the thread.
    pub fn iterative_search<const INFO: bool>(&mut self, t: &mut Thread, tt: &TT) {
        while t.depth < MAX_DEPTH && t.clock.start_search(t.depth + 1, t.nodes, t.best_move()) {
            let eval = self.aspiration_window(t, tt);

            if t.stop {
                break;
            }

            // Update thread data after a search finishes.
            t.eval = eval;
            t.depth += 1;
            if INFO {
                println!("{t}");
            }
        }
    }

    /// Aspiration Window loop
    /// Run searches on progressively wider windows until we find a value within the window.
    /// This may update the best move even though we do not fully complete the loop.
    fn aspiration_window(&mut self, t: &mut Thread, tt: &TT) -> Eval {
        let mut pv = PVTable::default();
        let mut new_depth = t.depth + 1;
        let mut alpha = -INFINITY;
        let mut beta = INFINITY;
        let mut delta = ASPIRATION_WINDOW;

        // Setup aspiration windows when searching a sufficient depth
        if new_depth >= ASPIRATION_LOWER_LIMIT {
            alpha = (-INFINITY).max(t.eval - delta);
            beta = (INFINITY).min(t.eval + delta);
        }

        loop {
            let eval = self.negamax::<true>(t, tt, &mut pv, alpha, beta, new_depth, false);
            if t.stop {
                return -INFINITY;
            }

            if eval <= alpha {
                // Fail-low: widen window down, reset depth, keep the old best move
                beta = (alpha + beta) / 2;
                alpha = (-INFINITY).max(alpha - delta);
                new_depth = t.depth + 1;
            } else if eval >= beta {
                // Fail-high: widen window up, reduce depth and save the best move
                beta = (INFINITY).min(beta + delta);
                t.pv = pv.clone();

                if eval.abs() < MATE_IN_PLY && new_depth > 1 {
                    new_depth -= 1;
                }
            } else {
                // Search within window, success
                t.pv = pv;
                return eval;
            }

            // Widen window, fully reopen when it's too wide
            delta += delta / 2;
            if delta >= BIG_DELTA {
                alpha = -INFINITY;
                beta = INFINITY;
            }
        }
    }

    /// Perform a Null-Window search to prove a position scores above/below the baseline eval.
    fn zw_search(
        &mut self,
        t: &mut Thread,
        tt: &TT,
        pv: &mut PVTable,
        eval: Eval,
        depth: usize,
        cutnode: bool,
    ) -> Eval {
        self.negamax::<false>(t, tt, pv, eval - 1, eval, depth, cutnode)
    }

    /// Standard alpha-beta negamax tree search
    #[allow(clippy::too_many_arguments)]
    fn negamax<const ROOT: bool>(
        &mut self,
        t: &mut Thread,
        tt: &TT,
        pv: &mut PVTable,
        mut alpha: Eval,
        mut beta: Eval,
        mut depth: usize,
        cutnode: bool,
    ) -> Eval {
        if t.stop || !t.clock.continue_search(t.nodes) {
            t.stop = true;
            return 0;
        }

        let pv_node = alpha != beta - 1;
        let in_check = self.king_in_check();

        let mut old_pv = PVTable::default();
        let opv = &mut old_pv;
        pv.length = 0;

        if ROOT {
            t.seldepth = 0;
        } else {
            t.seldepth = t.seldepth.max(t.ply);
        }

        // Check extension
        if in_check && depth < MAX_DEPTH {
            depth += 1;
        }

        // Quiescence search
        if depth == 0 || t.ply >= MAX_DEPTH {
            return self.quiescence(t, tt, alpha, beta);
        }

        if !ROOT {
            // Mate distance pruning
            // Shrink the window based on the best/worst possible outcomes, which are being mated
            // now or mating in the next ply. Prune if even these extreme situations would not
            // produce a line better than the PV
            alpha = alpha.max(-MATE + t.ply as Eval);
            beta = beta.min(MATE - t.ply as Eval - 1);
            if alpha >= beta {
                return alpha;
            }

            // Stop searching if the position is a rule-based draw
            if self.is_draw(t.ply_from_null) {
                return 0;
            }
        }

        // Singular Extension (first part):
        // If we are excluding a move to verify singularity, limit pruning in this node.
        // Otherwise, identify possibly singular moves based on the tt hit.
        let excluded = t.excluded[t.ply];
        let in_singular_search = excluded.is_some();
        let mut possible_singularity = false;

        // Probe tt for the best move and possible cutoffs.
        let tt_entry = tt.probe(self.board.hash);
        let mut tt_move = None;

        if let Some(entry) = tt_entry {
            // Don't use the tt result at the root of a singular search!
            if !in_singular_search {
                let tt_depth = entry.get_depth();
                let tt_flag = entry.get_flag();
                let tt_eval = entry.get_eval(t.ply);

                // TT Cutoffs
                if tt_depth >= depth && !pv_node {
                    match tt_flag {
                        TTFlag::Exact => return tt_eval,
                        TTFlag::Lower if tt_eval >= beta => return beta,
                        TTFlag::Upper if tt_eval <= alpha => return alpha,
                        _ => (),
                    }
                }

                tt_move = entry.get_move();
                possible_singularity = !ROOT
                    && depth >= SE_LOWER_LIMIT
                    && tt_eval.abs() < MATE_IN_PLY
                    && (tt_flag == TTFlag::Lower || tt_flag == TTFlag::Exact)
                    && tt_depth >= depth - 3;
            }
        }

        // Compute the static eval. Try to avoid re-computing it if we already have it in some form.
        // When in check, we keep -INFINITY.
        let mut stand_pat = -INFINITY;

        if in_singular_search {
            stand_pat = t.eval_stack[t.ply];
        } else if !in_check {
            // If we have a tt entry, use the static eval from there
            if let Some(entry) = tt_entry {
                let tt_flag = entry.get_flag();
                let tt_eval = entry.get_eval(t.ply);
                let tt_static_eval = entry.get_static_eval();

                if tt_static_eval == -INFINITY {
                    stand_pat = self.evaluate();
                } else {
                    stand_pat = tt_static_eval;
                }

                // If the tt eval is a tigher bound than the static eval, use it as stand pat
                if (tt_flag == TTFlag::Lower && tt_eval > tt_static_eval)
                    || (tt_flag == TTFlag::Upper && tt_eval <= tt_static_eval)
                {
                    stand_pat = tt_eval;
                }
            } else {
                // Without a tt entry (and not in check), we have to compute the static eval
                stand_pat = self.evaluate();

                // Chuck the static eval into the tt. This won't overwrite any relevant entry
                tt.insert(
                    self.board.hash,
                    TTFlag::None,
                    NULL_MOVE,
                    -INFINITY,
                    stand_pat,
                    0,
                    t.ply,
                    false,
                );
            }
        };

        t.eval_stack[t.ply] = stand_pat;

        // Improving is true when the current static eval is better than that of a move ago
        // Assuming this trend continues down this branch, we can prune high more aggressively,
        // while we should be less aggressive when pruning low.
        let improving = !in_check && t.ply > 1 && stand_pat > t.eval_stack[t.ply - 2];

        // Static pruning techniques:
        // these heuristics are trying to prove that the position is statically good enough to not
        // need any further deep search.
        if !pv_node && !in_check && !in_singular_search {
            // Reverse Futility Pruning (static eval pruning)
            // If the static eval is above beta by a certain margin at shallow depth, we can prune
            // assuming a beta cutoff. If the static eval is improving, we reduce the margin.
            let rfp_margin =
                RFP_MARGIN * (depth as Eval) - RFP_IMPROVING_MARGIN * (improving as Eval);
            if depth <= RFP_THRESHOLD && stand_pat - rfp_margin >= beta {
                return beta;
            }

            // Null Move Pruning (reduction value from CounterGO)
            // Give the opponent a "free shot" and see if that improves beta.
            if depth > NMP_LOWER_LIMIT
                && stand_pat + NMP_IMPROVING_MARGIN * (improving as Eval) >= beta
                && t.ply_from_null > 0
                && !self.only_king_pawns_left()
            {
                let r = (NMP_BASE + depth / NMP_FACTOR).min(depth);

                self.make_null(t);
                let eval = -self.zw_search(t, tt, opv, -(beta - 1), depth - r, !cutnode);
                self.undo_move(t);

                // cutoff above beta
                if eval >= beta {
                    return beta;
                }
            }
        }

        // Internal Iterative Reduction
        // Without a TT hit, it's better to do a reduced search to then setup the TT entry for the next
        // IID iteration.
        if !ROOT
            && depth >= IIR_LOWER_LIMIT
            && !in_singular_search
            && (tt_entry.is_none() || tt_entry.is_some_and(|e| e.get_flag() == TTFlag::None))
        {
            depth -= 1;
        }

        let mut picker = self.gen_moves::<QUIETS>(tt_move, 0);

        // Mate or stalemate. Don't save in the TT, simply return early
        if picker.stage == Stage::Done {
            if in_check {
                return -MATE + t.ply as Eval;
            } else {
                return 0;
            }
        };

        let old_alpha = alpha;
        let mut best_move = NULL_MOVE;
        let mut best_eval = -INFINITY;
        let mut searched_quiets = Vec::with_capacity(20);
        let mut move_count = 0;

        #[cfg(not(feature = "datagen"))]
        let lmp_count = LMP_BASE + (depth * depth);

        #[cfg(not(feature = "datagen"))]
        let see_margins = [
            SEE_CAPTURE_MARGIN * (depth * depth) as Eval,
            SEE_QUIET_MARGIN * depth as Eval,
        ];

        while let Some((m, s)) = picker.next(&self.board, t) {
            // Skip SE excluded move
            if excluded == Some(m) {
                move_count += 1;
                continue;
            }

            let start_nodes = t.nodes;
            let is_quiet = m.get_type().is_quiet();

            // Quiet move pruning
            #[cfg(not(feature = "datagen"))]
            if !pv_node && !in_check && !picker.skip_quiets && best_eval > -MATE_IN_PLY {
                // History leaf pruning
                // Below a certain depth, prune negative history moves in non-pv nodes
                if is_quiet && depth <= HLP_THRESHOLD && s < HLP_BASE {
                    picker.skip_quiets = true;
                }

                let lmr_depth = depth - lmr_reduction(depth, move_count).min(depth);

                // Extended Futility pruning
                // Below a certain depth, prune moves which will most likely not improve alpha
                let efp_margin = EFP_BASE + EFP_MARGIN * (lmr_depth as Eval);
                if lmr_depth <= EFP_THRESHOLD && stand_pat + efp_margin < alpha {
                    picker.skip_quiets = true;
                }

                // Late move pruning
                if depth <= LMP_THRESHOLD && move_count >= lmp_count {
                    picker.skip_quiets = true;
                }
            }

            // SEE pruning for captures and quiets
            #[cfg(not(feature = "datagen"))]
            if best_eval > -MATE_IN_PLY
                && depth <= SEE_PRUNING_THRESHOLD
                && picker.stage > Stage::GoodTacticals
                && !self.board.see(m, see_margins[is_quiet as usize])
            {
                move_count += 1;
                continue;
            };

            // Singular Extensions (second part):
            // We perform a verification search excluding the tt move, with a window around se_beta.
            // Failing below the reduced beta means no other move is any good.
            let mut ext_depth = depth;
            if possible_singularity && s == TT_SCORE {
                let tt_eval = tt_entry.unwrap().get_eval(t.ply); // Can't panic
                let se_beta = (tt_eval - 2 * depth as Eval).max(-INFINITY);
                let se_depth = (depth - 1) / 2; // depth is always > 0 so this is safe

                t.excluded[t.ply] = Some(m);
                let eval = self.zw_search(t, tt, opv, se_beta, se_depth, cutnode);
                t.excluded[t.ply] = None;

                if eval < se_beta {
                    ext_depth += 1;
                }
            }

            self.make_move(m, t);
            tt.prefetch(self.board.hash); // prefetch next hash

            // Principal Variation Search + Late Move Reductions
            // Before most searches, we run a "verification" search on a null window to prove it
            // fails high on alpha. If it doesn't, it's likely a cutnode.
            // We reduce the depth of these searches the further in the move list we go.
            let mut eval = -INFINITY;
            let full_depth_search =
                if depth >= LMR_LOWER_LIMIT && move_count >= LMR_THRESHOLD + pv_node as usize {
                    let r = if is_quiet {
                        let mut r = lmr_reduction(depth, move_count) as i32;
                        let is_check = self.king_in_check();

                        r += !pv_node as i32; // reduce more in non-pv nodes
                        r += cutnode as i32; // reduce more for cutnodes

                        r -= in_check as i32; // reduce less when in check
                        r -= is_check as i32; // reduce less when giving check

                        if s > HISTORY_MAX / 2 {
                            r -= 1; // Reduce less high history moves/killers
                        } else if s < -HISTORY_MAX / 2 {
                            r += 1; // Reduce more low history moves
                        }

                        r.clamp(1, (depth - 1) as i32) as usize
                    } else {
                        1
                    };

                    // Reduced depth null window search
                    // Since we are speculating being an allnode, expect the child to be a cutnode
                    eval = -self.zw_search(t, tt, opv, -alpha, ext_depth - r, true);
                    eval > alpha && r > 1
                } else {
                    !pv_node || move_count > 0
                };

            // Full depth null window search when lmr fails or when using pvs
            // Allnodes/Cutnodes alternate
            if full_depth_search {
                eval = -self.zw_search(t, tt, opv, -alpha, ext_depth - 1, !cutnode);
            }

            // Full depth full window search for the first move of all PV nodes and when pvs fails
            // We expect the child node to be a PV node
            if pv_node && (move_count == 0 || eval > alpha) {
                eval = -self.negamax::<false>(t, tt, opv, -beta, -alpha, ext_depth - 1, false);
            }

            self.undo_move(t);

            if t.stop {
                return 0;
            }

            // In root, update the node counts used by the clock
            if ROOT {
                t.clock.update_node_counts(m, t.nodes - start_nodes);
            }

            if eval > best_eval {
                best_eval = eval;

                if eval > alpha {
                    best_move = m;
                    alpha = eval;
                    pv.update_pv_line(m, opv);
                }

                if eval >= beta {
                    if is_quiet {
                        t.update_tables(m, depth, self.board.side, searched_quiets);
                    };

                    alpha = beta;
                    break;
                }
            }

            // save searched quiets that didn't cause a cutoff for negative history score
            if is_quiet {
                searched_quiets.push(m);
            }

            move_count += 1;
        }

        if !t.stop {
            let tt_flag = if best_eval >= beta {
                TTFlag::Lower
            } else if best_eval > old_alpha {
                TTFlag::Exact
            } else {
                TTFlag::Upper
            };

            tt.insert(
                self.board.hash,
                tt_flag,
                best_move,
                alpha,
                stand_pat,
                depth,
                t.ply,
                pv_node,
            );
        }

        alpha
    }

    /// Quiescence search: only search captures to avoid the horizon effect
    fn quiescence(&mut self, t: &mut Thread, tt: &TT, mut alpha: Eval, beta: Eval) -> Eval {
        if t.stop || !t.clock.continue_search(t.nodes) {
            t.stop = true;
            return 0;
        }

        t.seldepth = t.seldepth.max(t.ply);

        // Return early when reaching max depth
        if t.ply >= MAX_DEPTH {
            return self.evaluate();
        }

        let in_check = self.king_in_check();

        // Probe the TT and if possible get a tt move
        let tt_entry = tt.probe(self.board.hash);
        let mut tt_move = None;

        if let Some(entry) = tt_entry {
            let tt_eval = entry.get_eval(t.ply);

            // TT Cutoffs
            match entry.get_flag() {
                TTFlag::Exact => return tt_eval,
                TTFlag::Lower if tt_eval >= beta => return beta,
                TTFlag::Upper if tt_eval <= alpha => return alpha,
                _ => (),
            }

            tt_move = entry.get_move();
        };

        // Compute the static eval when not in check
        let mut stand_pat = -INFINITY;

        if !in_check {
            if let Some(entry) = tt_entry {
                let tt_flag = entry.get_flag();
                let tt_eval = entry.get_eval(t.ply);
                let tt_static_eval = entry.get_static_eval();

                if tt_static_eval == -INFINITY {
                    stand_pat = self.evaluate();
                } else {
                    stand_pat = tt_static_eval;
                }

                // If the tt eval is a tigher bound than the static eval, use it as stand pat
                if (tt_flag == TTFlag::Lower && tt_eval > tt_static_eval)
                    || (tt_flag == TTFlag::Upper && tt_eval <= tt_static_eval)
                {
                    stand_pat = tt_eval;
                }
            } else {
                stand_pat = self.evaluate();
            }
        };

        // Stand pat pruning
        let old_alpha = alpha;
        alpha = alpha.max(stand_pat);

        if stand_pat >= beta {
            return stand_pat;
        }

        let mut best_move = NULL_MOVE;
        let mut best_eval = stand_pat;
        let mut picker = self.gen_moves::<CAPTURES>(tt_move, 0);

        // The capture picker implicitly prunes bad SEE moves
        while let Some((m, _)) = picker.next(&self.board, t) {
            self.make_move(m, t);
            tt.prefetch(self.board.hash); // prefetch next hash
            let eval = -self.quiescence(t, tt, -beta, -alpha);
            self.undo_move(t);

            if t.stop {
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
        if !t.stop {
            let tt_flag = if best_eval >= beta {
                TTFlag::Lower
            } else if best_eval > old_alpha {
                TTFlag::Exact
            } else {
                TTFlag::Upper
            };

            tt.insert(
                self.board.hash,
                tt_flag,
                best_move,
                alpha,
                stand_pat,
                0,
                t.ply,
                false,
            );
        }

        alpha
    }
}

/// Test nodes searched
/// Run with: cargo test --release search -- --show-output
#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn search_suite() {
        #[rustfmt::skip]
        const SEARCH_SUITE: [(&str, &str, usize); 4] = [
            ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", "Kiwipete", 15),
            ("rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1", "Killer", 15),
            ("8/8/8/2K5/5Q2/8/4k3/8 w - - 0 1", "Mate in 4", 20),
            ("1b2k3/3rP3/2B1K3/8/5P2/8/1p6/8 b - - 4 57", "All moves lead to mate", 5),
        ];

        init_all_tables();

        for (fen, name, depth) in SEARCH_SUITE {
            println!("Searching: {}", name);

            let mut position: Position = format!("fen {}", fen).parse().unwrap();
            let mut t = Thread::fixed_depth(depth);

            println!("\n{}\n\n", position.board);

            let start = Instant::now();
            position.iterative_search::<true>(&mut t, &TT::default());
            let duration = start.elapsed();

            println!(
                "\nDEPTH: {depth} Found {} in {duration:?}\n--------------------------------\n",
                t.best_move(),
            );
        }
    }
}
