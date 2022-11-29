//! # Search the move tree
//! 
//! Alpha : lower bound, the lowest score the current player is assured of
//! Beta  : upper bound, the highest score the opposite player is assured of
//! 
//! The current player wants a high score, the opposite player wants a low score
//! 

use crate::{
    Board,
    Tables,
    moves::*,
    move_order::*,
    evaluation::*,
};

pub const MAX_DEPTH: usize = 128;
const LMR_THRESHOLD: u32 = 4;     // moves to execute before any reduction
const LMR_LOWER_LIMIT: usize = 3; // stop applying lmr near leaves
const NMP_REDUCTION: usize = 2;   // null move pruning reduced depth

pub struct Search<'a>{
    tables: &'a Tables,
    sorter: MoveSorter,
    nodes: u32,
}

impl <'a> Search<'a>{
    pub fn new(tables: &'a Tables) -> Search {
        Search {
            tables,
            sorter: MoveSorter::new(),
            nodes: 0,
        }
    }

    pub fn iterative_search(&mut self, board: &Board, depth: usize) -> Move {
        for d in 1..=depth {
            let eval = self.negamax(board, MIN, MAX, d, 0);
    
            print!("info score cp {} depth {} nodes {} pv ", eval, d, self.nodes);

            for m in self.sorter.get_pv() { print!("{} ", m); }
            println!();
        }

        self.sorter.get_pv()[0]
    }
    
    fn negamax(
        &mut self,
        board: &Board,
        mut alpha: Eval,
        beta: Eval,
        mut depth: usize,
        ply: usize,
    ) -> Eval {
        self.nodes += 1;
        self.sorter.update_pv_lenghts(ply);

        // Quiescence search to avoid horizon effect
        if depth == 0 { return self.quiescence(board, alpha, beta, ply); }
    
        let mut eval: Eval;
        let mut moves_checked: u32 = 0;
        let check_flag = board.king_in_check(self.tables);
        
        // Extend search depth when king is in check
        if check_flag { 
            depth += 1;
        } else if depth > NMP_REDUCTION {
            // Apply Null move pruning
            let nmp: Board = board.make_null_move();

            eval = -self.negamax(&nmp, -beta, -beta + 1, depth - 1 - NMP_REDUCTION, ply + 1);
            if eval >= beta {
                return beta;
            }
        };
        
        let moves: Vec<Move> = self.sorter.sort_moves(board.generate_moves(self.tables), ply);
        for m in moves {
            // only consider legal moves
            if let Some(new) = board.make_move(m, self.tables) {

                if moves_checked == 0 { // full depth search
                    eval = -self.negamax(&new, -beta, -alpha, depth - 1, ply + 1);
                } else {
                    // apply lmr for non-pv nodes
                    let mut lmr_depth = depth;
                    if  moves_checked >= LMR_THRESHOLD  &&
                        depth >= LMR_LOWER_LIMIT        &&
                        !check_flag                     &&
                        !m.is_capture()                 &&
                        !m.is_promotion()
                    {
                        lmr_depth -= 1; 
                    };

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
                
                if eval >= beta { // beta cutoff
                    if !(m.is_capture()) {
                        self.sorter.add_killer(m, ply);
                        self.sorter.add_history(m, depth);
                    };
                    
                    return beta;
                }

                if eval > alpha { // pv node
                    self.sorter.update_pv(m, ply);
                    alpha = eval; 
                }

                moves_checked += 1;
            }
        };
    
        if moves_checked == 0 {     // no legal moves
            if check_flag {
                MATE + ply as Eval  // checkmate
            } else {
                0                   // stalemate
            }
        } else {
            alpha
        }
    }
    
    fn quiescence(
        &mut self,
        board: &Board,
        mut alpha: Eval,
        beta: Eval,
        ply: usize,
    ) -> Eval {
        self.nodes += 1;
        let eval: Eval = evaluate(board);
    
        if eval >= beta { return beta; };       // beta cutoff
        if eval > alpha { alpha = eval; }       // pv node
    
        let moves: Vec<Move> = self.sorter.sort_captures(board.generate_moves(self.tables));
        for m in moves {
            if let Some(new) = board.make_move(m, self.tables) {
                let eval = - self.quiescence(&new, -beta, -alpha, ply + 1);
                
                if eval >= beta { return beta; };   // beta cutoff
                if eval > alpha { alpha = eval; };  // pv node
            }
        }

        alpha // node fails low
    }
}