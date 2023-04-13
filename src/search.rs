use std::cmp::{max, min};

use crate::clock::*;
use crate::move_sorter::*;
use crate::moves::*;
use crate::position::*;
use crate::search_params::*;
use crate::tables::*;
use crate::tt::*;

pub const INFO: bool = true;
pub const NO_INFO: bool = false;

/// Search the move tree, starting at the given position
pub struct Search<'a> {
    position: Position,
    clock: Clock,
    tt: &'a TT,
    pub nodes: u64,
    seldepth: usize,
    stop: bool,
}

impl<'a> Search<'a> {
    pub fn new(position: Position, clock: Clock, tt: &'a TT) -> Search<'a> {
        Search {
            position,
            clock,
            tt,
            nodes: 0,
            seldepth: 0,
            stop: false,
        }
    }

    /// Iteratively searches the board at increasing depth
    /// After the shallower depths, we start doing reduced-window searches and eventually reopen
    /// each "side" of the window in case of fail-high or fail-low
    pub fn iterative_search<const INFO: bool>(&mut self) -> (Move, Eval, usize) {
        let mut best_move = NULL_MOVE;
        let mut best_eval = -MAX;
        let mut temp_best: Move;
        let mut alpha = -MAX;
        let mut beta = MAX;
        let mut eval;
        let mut depth = 1;

        while !self.stop && self.clock.start_check(depth, self.nodes) && depth < MAX_DEPTH {
            (eval, temp_best) = self.search_root(alpha, beta, depth);

            if eval <= alpha {
                alpha = -MAX;
            } else if eval >= beta {
                beta = MAX;
            } else {
                if depth >= ASPIRATION_THRESHOLD {
                    alpha = eval - ASPIRATION_WINDOW;
                    beta = eval + ASPIRATION_WINDOW;
                }

                if !self.stop {
                    if INFO {
                        self.print_info(eval, depth);
                        self.seldepth = 0;
                    }
                    best_move = temp_best;
                    best_eval = eval;
                    depth += 1;
                }
            }
        }

        (best_move, best_eval, depth - 1)
    }

    // Separate function for searching the root. Saves temporary tt entries for root moves and
    // avoids a few optimizations. Allows returning the best move without pv retrieval.
    // Will be useful in case of future implementations of various
    // root-only heuristics
    fn search_root(&mut self, mut alpha: Eval, beta: Eval, mut depth: usize) -> (Eval, Move) {
        let in_check = self.position.king_in_check();
        if in_check {
            depth += 1;
        }

        // Probe tt only for best move
        match self.tt.probe(self.position.hash()) {
            Some(entry) => self.position.set_tt_move(Some(entry.get_move())),
            None => self.position.set_tt_move(None),
        };

        let mut eval: Eval;
        let mut best_move = NULL_MOVE;
        let mut searched_quiets = Vec::with_capacity(20);
        let mut tt_entry = TTField::new(&self.position, TTFlag::Upper, best_move, -MAX, depth);

        for (move_count, (m, _)) in self.position.generate_moves().enumerate() {
            self.position.make_move(m);
            if move_count == 0 {
                // full search on first move
                eval = -self.negamax(-beta, -alpha, depth - 1);
                best_move = m;
            } else {
                // use plain pvs without reductions in root
                eval = -self.negamax(-alpha - 1, -alpha, depth - 1);
                if eval > alpha && eval < beta && !self.stop {
                    eval = -self.negamax(-beta, -alpha, depth - 1);
                }
            };
            self.position.undo_move();

            if self.stop {
                return (0, NULL_MOVE);
            }

            let is_quiet = !m.is_capture() && !m.is_promotion();

            if eval > alpha {
                best_move = m;
                alpha = eval;

                if eval >= beta {
                    if is_quiet {
                        self.position.update_sorter(m, depth, searched_quiets);
                    };

                    tt_entry.update_data(TTFlag::Lower, best_move, beta);
                    self.tt.insert(tt_entry);

                    return (beta, best_move);
                }

                // in root, we insert partial results for the other threads to use
                tt_entry.update_data(TTFlag::Upper, best_move, alpha);
                self.tt.insert(tt_entry);
            }

            if is_quiet {
                searched_quiets.push(m);
            }
        }

        if !self.stop {
            tt_entry.update_data(TTFlag::Exact, best_move, alpha);
            self.tt.insert(tt_entry);
        }

        (alpha, best_move)
    }

    /// Fail-Hard Negamax search
    fn negamax(&mut self, mut alpha: Eval, mut beta: Eval, mut depth: usize) -> Eval {
        if self.stop || !self.clock.mid_check() {
            self.stop = true;
            return 0;
        }

        let pv_node = alpha != beta - 1; // False when searching with a null window
        let in_check = self.position.king_in_check();

        // Check extension
        if in_check {
            depth += 1;
        }

        if depth == 0 {
            return self.quiescence(alpha, beta);
        }
        self.nodes += 1;

        // Mate distance pruning (it's a bit faster to do this before draw detection)
        alpha = max(-MAX + self.position.ply as Eval, alpha);
        beta = min(MAX - self.position.ply as Eval - 1, beta);
        if alpha >= beta {
            return alpha;
        }

        // Stop searching if the position is a rule-based draw
        if self.position.is_draw() {
            return 0;
        }

        // Probe tt for eval and best move (for pv, only cutoff at leaves)
        match self.tt.probe(self.position.hash()) {
            Some(entry) => {
                let tt_move = entry.get_move();

                if pv_node {
                    if depth == 1 && entry.get_flag() == TTFlag::Exact {
                        return entry.get_value(self.position.ply);
                    }
                } else if entry.get_depth() >= depth {
                    let tt_eval = entry.get_value(self.position.ply);

                    match entry.get_flag() {
                        TTFlag::Exact => return tt_eval,
                        TTFlag::Upper => beta = min(beta, tt_eval),
                        TTFlag::Lower => alpha = max(alpha, tt_eval),
                    }

                    // Upper/Lower flags can cause indirect cutoffs!
                    if alpha >= beta {
                        return tt_eval;
                    }
                }
                self.position.set_tt_move(Some(tt_move));
            }

            None => self.position.set_tt_move(None),
        };

        // Static pruning techniques:
        // these heuristics are trying to prove that the position is statically good enough to not
        // need any further deep search.
        let mut stand_pat = 0;

        if !pv_node && !in_check {
            // Reverse Futility Pruning (static eval pruning)
            // At pre-frontier nodes, check if the static eval minus a safety margin is enough to
            // produce a beta cutoff.
            if depth <= RFP_THRESHOLD {
                stand_pat = self.position.evaluate(); // this remains valid for efp

                if !is_mate(beta.abs()) && stand_pat - RFP_MARGIN * (depth as Eval) >= beta {
                    return beta;
                }
            }

            // Null Move Pruning (reduction value from CounterGO)
            // Give the opponent a "free shot" and see if that improves beta.
            if depth > NMP_LOWER_LIMIT && !self.position.only_king_pawns_left() {
                let reduction = NMP_BASE_R + depth / NMP_FACTOR;

                self.position.make_null();
                let eval = -self.negamax(-beta, -beta + 1, depth.saturating_sub(reduction));
                self.position.undo_move();

                // cutoff above beta and not for mate scores!
                if eval >= beta && !is_mate(eval.abs()) {
                    return beta;
                }
            }
        }

        // Internal Iterative Reduction
        // When no TT Move is found in any node, apply a 1-ply reduction
        if depth >= IIR_LOWER_LIMIT && !self.position.found_tt_move() {
            depth -= 1;
        }

        let move_list = self.position.generate_moves();

        // Mate or stalemate. Don't save in the TT, simply return early
        if move_list.is_empty() {
            if in_check {
                return -MAX + self.position.ply as Eval;
            } else {
                return 0;
            }
        };

        let mut eval: Eval;
        let mut searched_quiets = Vec::with_capacity(20);
        let mut tt_field = TTField::new(&self.position, TTFlag::Upper, NULL_MOVE, alpha, depth);

        for (move_count, (m, s)) in move_list.enumerate() {
            self.position.make_move(m);

            // Flag for moves checking the opponent
            let is_check = self.position.king_in_check();
            let is_quiet = !m.is_capture() && !m.is_promotion();

            // Quiet move pruning
            if !pv_node && !in_check && is_quiet && !is_check && !is_mate(alpha.abs()) {
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
                    self.position.undo_move();

                    if move_count == 0 {
                        tt_field.update_data(TTFlag::Upper, m, alpha);
                    }
                    break;
                }
            }

            if move_count == 0 {
                // full depth search on first move
                eval = -self.negamax(-beta, -alpha, depth - 1);
                tt_field.update_data(TTFlag::Upper, m, alpha); // always save at least one move
            } else {
                // reduce depth for all moves beyond first
                let reduced_depth =
                    if move_count >= LMR_THRESHOLD && depth >= LMR_LOWER_LIMIT && is_quiet {
                        let lmr_red = lmr_reduction(depth, move_count) as i32;
                        let lmr_ext = is_check as i32 + in_check as i32 + pv_node as i32;

                        (depth as i32 + lmr_ext - lmr_red).clamp(1, depth as i32) as usize
                    } else {
                        depth
                    };

                // do reduced depth pvs search, and eventually fall back to full window
                eval = -self.negamax(-alpha - 1, -alpha, reduced_depth - 1);
                if eval > alpha && pv_node && !self.stop {
                    eval = -self.negamax(-beta, -alpha, reduced_depth - 1);
                }

                // fall back to full depth if lmr failed
                if reduced_depth < depth && eval > alpha && !self.stop {
                    eval = -self.negamax(-alpha - 1, -alpha, depth - 1);
                    if eval > alpha && pv_node && !self.stop {
                        eval = -self.negamax(-beta, -alpha, depth - 1);
                    }
                }
            };

            self.position.undo_move();

            if self.stop {
                return 0;
            }

            if eval > alpha {
                if eval >= beta {
                    if is_quiet {
                        self.position.update_sorter(m, depth, searched_quiets);
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

        if !self.stop {
            self.tt.insert(tt_field);
        }

        alpha
    }

    /// Quiescence search (only look at capture moves)
    fn quiescence(&mut self, mut alpha: Eval, beta: Eval) -> Eval {
        if self.stop || !self.clock.mid_check() {
            self.stop = true;
            return 0;
        }

        self.nodes += 1;
        self.seldepth = max(self.seldepth, self.position.ply);

        // Stand pat and delta pruning
        let stand_pat = self.position.evaluate();

        if self.position.ply >= MAX_DEPTH {
            return stand_pat;
        }

        if stand_pat >= beta {
            return beta;
        }
        if stand_pat < alpha - QS_DELTA_MARGIN {
            return alpha;
        }
        alpha = max(stand_pat, alpha);

        let in_check = self.position.king_in_check();
        for (m, s) in self.position.generate_captures() {
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

            self.position.make_move(m);
            let eval = -self.quiescence(-beta, -alpha);
            self.position.undo_move();

            if self.stop {
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

    /// Print pv by traversing the tt from the root
    fn print_pv(&self, depth: usize) {
        let mut board = self.position.board;

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
    fn print_info(&self, eval: Eval, depth: usize) {
        let score = if is_mate(eval) {
            format!("mate {} ", (MAX - eval + 1) / 2)
        } else if is_mate(-eval) {
            format!("mate {} ", -(MAX + eval) / 2)
        } else {
            format!("cp {eval} ")
        };

        let time = max(self.clock.elapsed().as_millis(), 1);

        print!(
            "info time {} score {} depth {} seldepth {} nodes {} nps {} pv",
            time,
            score,
            depth,
            self.seldepth,
            self.nodes,
            (self.nodes as u128 * 1000) / time,
        );

        self.print_pv(depth);
    }
}

/// Test nodes searched
/// Run with: cargo test --release search -- --show-output
#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::sync::{atomic::AtomicBool, Arc};
    use std::time::Instant;

    use crate::piece::*;

    fn search_driver(fen: &str, depth: usize) {
        init_all_tables();
        let position: Position = fen.parse().unwrap();
        let tt = TT::default();
        let clock = Clock::new(
            TimeControl::FixedDepth(depth),
            Arc::new(AtomicBool::new(false)),
            position.board.side == Color::White,
        );

        println!("\n{}\n\n", position.board);

        let mut search: Search = Search::new(position, clock, &tt);
        let start = Instant::now();
        let (best_move, _, _) = search.iterative_search::<INFO>();
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
