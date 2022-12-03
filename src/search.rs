//! # Search the move tree
//! 
//! Alpha : lower bound, the lowest score the current player is assured of
//! Beta  : upper bound, the highest score the opposite player is assured of
//! 
//! The current player wants a high score, the opposite player wants a low score
//!  ... (will explain in more detail)

use std::cmp::{ max, min };
use crate::{
    board_repr::Board,
    tables::Tables,
    moves::*,
    move_order::*,
    evaluation::*,
    tt::*,
};

pub const MAX_DEPTH: usize = 128;
const LMR_THRESHOLD: u32 = 4;     // moves to execute before any reduction
const LMR_LOWER_LIMIT: usize = 3; // stop applying lmr near leaves
const NMP_REDUCTION: usize = 2;   // null move pruning reduced depth

const ASPIRATION_WINDOW: Eval = 50;    // aspiration window width
const ASPIRATION_THRESHOLD: usize = 4; // depth at which windows are reduced

const FUTILITY_MARGIN: Eval = 1100;    // highest queen value possible

pub struct Search<'a>{
    tt: &'a mut TT,
    tables: &'a Tables,
    sorter: MoveSorter,
    step: u8,
    nodes: u32,
}

impl <'a> Search<'a>{
    pub fn new(tt: &'a mut TT, tables: &'a Tables) -> Search<'a> {
        Search {
            tt,
            tables,
            sorter: MoveSorter::new(),
            step: 0,
            nodes: 0,
        }
    }

    pub fn recover_pv(&self, mut board: Board) -> Vec<Move> {
        let mut pv: Vec<Move> = Vec::new();

        // traverse down the tree through the trasposition table
        loop {
            let entry = match self.tt.probe(board.hash) {
                Some(e) => e,
                None => break,
            };

            let tt_move = match entry.get_best_move() {
                Some(m) => m,
                None => break,
            };

            let moves = board.generate_moves(self.tables).all_moves;
            if moves.contains(&tt_move) {
                board = match board.make_move(tt_move, self.tables) {
                    Some(b) => {
                        pv.push(tt_move);
                        b
                    }
                    None => break,
                }
            } else {
                break;
            }
        }
        
        pv
    }

    pub fn iterative_search(&mut self, board: &Board, depth: usize) -> Move {
        let mut alpha: Eval = MIN;
        let mut beta : Eval = MAX;
        let mut eval: Eval = 0;
        let mut pv: Vec<Move> = vec![NULL_MOVE];

        for d in 1..=depth {
            if d < ASPIRATION_THRESHOLD {
                // don't apply aspiration windows to shallow searches (< 4 ply deep)
                eval = self.negamax(board, alpha, beta, d, 0);
            } else {
                // reduce window using previous eval
                alpha = eval - ASPIRATION_WINDOW;
                beta  = eval + ASPIRATION_WINDOW;
                eval = self.negamax(board, alpha, beta, d, 0);
                
                if eval < alpha || eval > beta {
                    // reduced window search failed
                    alpha = MIN;
                    beta = MAX;
                    eval = self.negamax(board, alpha, beta, d, 0);
                }
            }
            
            print!("info score cp {} depth {} nodes {} pv ", eval, d, self.nodes);
            pv = self.recover_pv(board.clone());
            
            for m in &pv { print!("{} ", m); }
            println!();
            
            self.step += 1;
        }

        pv[0]
    }
    
    fn negamax(
        &mut self,
        board: &Board,
        mut alpha: Eval,
        mut beta: Eval,
        mut depth: usize,
        ply: usize,
    ) -> Eval {
        let mut best_eval: Eval = MIN;
        let mut eval: Eval;
        let in_check = board.king_in_check(self.tables);

        self.nodes += 1;
        
        // Mate distance pruning
        alpha = max(-MATE + ply as Eval, alpha);
        beta  = min( MATE - ply as Eval - 1, beta);
        if alpha >= beta { return alpha; }
        
        // Probe tt for eval and best move
        match self.tt.probe(board.hash) {
            Some(entry) => {
                if entry.depth >= depth as u8 {
                    let tt_eval = entry.get_value(ply);
        
                    match entry.flag {
                        TTFlag::Exact => return tt_eval,
                        TTFlag::Upper => beta = min(beta, tt_eval),
                        TTFlag::Lower => alpha = max(alpha, tt_eval),
                    }

                    // Upper/Lower flags can cause indirect cutoffs!
                    if alpha >= beta { return tt_eval; }
                }
                self.sorter.tt_move = entry.get_best_move();
            }

            None => self.sorter.tt_move = None
        };

        // False when in a PVS node
        let pv_node: bool = alpha != beta - 1;

        // Extend search depth when king is in check
        if in_check { 
            depth += 1;
        } else if depth > NMP_REDUCTION  && !pv_node {
            // Apply Null move pruning
            let nmp: Board = board.make_null_move();
            
            eval = -self.negamax(&nmp, -beta, -beta + 1, depth - 1 - NMP_REDUCTION, ply + 1);
            if eval >= beta { return beta; }
        }

        // Quiescence search to avoid horizon effect
        if depth == 0 { return self.quiescence(board, alpha, beta, ply); }
    
        // Main recursive search block
        let moves: Vec<Move> = self.sorter.sort_moves(board.generate_moves(self.tables), ply);
        let mut moves_checked: u32 = 0;
        let mut best_move: Move = NULL_MOVE;
        let mut tt_bound: TTFlag = TTFlag::Upper;

        for m in moves {
            // only consider legal moves
            if let Some(new) = board.make_move(m, self.tables) {
                moves_checked += 1;

                if moves_checked == 1 { // full depth search on first move
                    eval = -self.negamax(&new, -beta, -alpha, depth - 1, ply + 1);
                } else {
                    // apply lmr for non-pv nodes
                    let mut lmr_depth = depth;
                    if  moves_checked >= LMR_THRESHOLD  &&
                        depth >= LMR_LOWER_LIMIT        &&
                        !in_check                       &&
                        !m.is_capture()                 &&
                        !m.is_promotion()
                    {
                        lmr_depth -= 1; 
                    }

                    // try applying LMR + PVS
                    eval = -self.negamax(&new, -alpha - 1, -alpha, lmr_depth - 1, ply + 1);
                    if eval > alpha && eval < beta {            
                        // PVS failed, retry reduced depth full window
                        eval = -self.negamax(&new, -beta, -alpha, lmr_depth - 1, ply + 1);

                        if eval > alpha && lmr_depth < depth {
                            // LMR failed, retry full search.
                            eval = -self.negamax(&new, -beta, -alpha, depth - 1, ply + 1);
                        };
                    };
                };

                best_eval = max(best_eval, eval);
                
                if eval > alpha {               // possible pv node
                    best_move = m;
    
                    if eval >= beta {           // beta cutoff
                        if !(m.is_capture()) {
                            self.sorter.add_killer(m, ply);
                            self.sorter.add_history(m, depth);
                        };
    
                        tt_bound = TTFlag::Lower;
                        break;
                    }

                    alpha = eval;
                    tt_bound = TTFlag::Exact;
                }
            }
        };
    
        if moves_checked == 0 {     // no legal moves
            if in_check {
                best_eval = -MATE + ply as Eval  // checkmate
            } else {
                best_eval = 0                   // stalemate
            }
        };

        let tt_entry = TTField::new(
            board.hash,
            best_move,
            depth,
            self.step,
            best_eval,
            tt_bound,
            ply
        );

        self.tt.insert(tt_entry);

        best_eval
    }
    
    fn quiescence(
        &mut self,
        board: &Board,
        mut alpha: Eval,
        beta: Eval,
        ply: usize,
    ) -> Eval {
        self.nodes += 1;
        let mut best_eval: Eval = evaluate(board);   // try stand pat
        
        if best_eval >= beta { return best_eval; };       // beta cutoff
        if best_eval < alpha - FUTILITY_MARGIN { return alpha; } // delta pruning
        if best_eval > alpha { alpha = best_eval; }  // stand pat is pv
    
        let moves: Vec<Move> = self.sorter.sort_captures(board.generate_moves(self.tables));
        for m in moves {
            if let Some(new) = board.make_move(m, self.tables) {
                let eval = - self.quiescence(&new, -beta, -alpha, ply + 1);
                
                best_eval = max(best_eval, eval);
                if eval >= beta { return best_eval; }            // beta cutoff
                else if eval > alpha { alpha = eval; };     // possible pv node
            }
        }

        best_eval // node fails low
    }
}

/// Test nodes searched
/// Run with: cargo test --release search -- --show-output
#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::time::Instant;
    
    use crate::board_repr::*;

    #[test]
    fn search_kiwipete10() {
        let board: Board = Board::try_from(KIWIPETE_FEN).unwrap();
        let tables: Tables = Tables::default();
        let mut tt: TT = TT::default();
        let mut search: Search = Search::new(&mut tt, &tables);
        let depth = 10;

        println!("\n --- KIWIPETE POSITION ---\n{}\n\n", board);

        let start = Instant::now();
        let best_move = search.iterative_search(&board, depth);
        let duration = start.elapsed();

        println!("\nDEPTH: {} Found {} in {:?}\n--------------------------------\n", depth, best_move, duration);
    }

    #[test]
    fn search_killer10() {
        let board: Board = Board::try_from(KILLER_FEN).unwrap();
        let tables: Tables = Tables::default();
        let mut tt: TT = TT::default();
        let mut search: Search = Search::new(&mut tt, &tables);
        let depth = 10;

        println!("\n --- KILLER POSITION ---\n{}\n\n", board);

        let start = Instant::now();
        let best_move = search.iterative_search(&board, depth);
        let duration = start.elapsed();

        println!("\nDEPTH: {} Found {} in {:?}\n--------------------------------\n", depth, best_move, duration);
    }
}