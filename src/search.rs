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
    zobrist::ZHash,
};

pub const MAX_DEPTH: u8 = 128;
const LMR_THRESHOLD: u32 = 4;  // moves to execute before any reduction
const LMR_LOWER_LIMIT: u8 = 3; // stop applying lmr near leaves
const NMP_REDUCTION: u8 = 2;   // null move pruning reduced depth

const ASPIRATION_WINDOW: Eval = 50; // aspiration window width
const ASPIRATION_THRESHOLD: u8 = 4; // depth at which windows are reduced

const FUTILITY_MARGIN: Eval = 1100; // highest queen value possible

pub struct Search<'a>{
    history: Vec<ZHash>,
    tt: &'a mut TT,
    tables: &'a Tables,
    sorter: MoveSorter,
    step: u8,
    nodes: u32,
}

impl <'a> Search<'a>{
    pub fn new(history: Vec<ZHash>, tt: &'a mut TT, tables: &'a Tables) -> Search<'a> {
        Search {
            history,
            tt,
            tables,
            sorter: MoveSorter::new(),
            step: 0,
            nodes: 0,
        }
    }

    /// Probe tt for best move in given board state (updates board)
    fn get_best_move(&self, board: &mut Board) -> Option<Move> {
        let entry = match self.tt.probe(board.hash) {
            Some(e) => e,
            None => return None,
        };

        let tt_move = match entry.get_best_move() {
            Some(m) => m,
            None => return None,
        };

        let moves: Vec<Move> = board.generate_moves(self.tables).all_moves;
        if moves.contains(&tt_move) {
            match board.make_move(tt_move, self.tables) {
                Some(b) => {
                    *board = b;

                    Some(tt_move)
                },

                None => None,
            }
        } else {
            None
        }
    }

    /// Recover pv from transposition table
    fn recover_pv(&self, mut board: Board, depth: u8) -> Vec<Move> {
        let mut pv: Vec<Move> = Vec::new();

        // traverse down the tree through the trasposition table
        for _ in 0..depth {
            let best_move = self.get_best_move(&mut board);

            match best_move {
                Some(m) => pv.push(m),
                None => break,
            }
        }
        pv
    }

    fn print_info(&self, board: Board, eval: Eval, depth: u8) {
        print!("info score ");

        if is_mate(eval) {          // mating
            print!("mate {} ", (MATE_VALUE - eval) / 2 + 1);
        } else if is_mated(eval) {  // mated
            print!("mate {} ", -(eval + MATE_VALUE) / 2 - 1);
        } else {
            print!("cp {} ", eval);
        }

        print!("depth {} nodes {} pv ", depth, self.nodes);

        let pv = self.recover_pv(board, depth);
        for m in &pv { print!("{} ", m); }
        println!();
    }

    pub fn iterative_search(&mut self, board: &Board, depth: u8) -> Move {
        let mut alpha: Eval = MIN;
        let mut beta : Eval = MAX;
        let mut eval: Eval = 0;

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

            self.print_info(board.clone(), eval, d);
            self.step += 1;
        }

        self.get_best_move(&mut board.clone()).unwrap()
    }
    
    // fn search_root(
    //     &mut self,
    //     mut alpha: Eval,
    //     beta: Eval,
    //     depth: u8,
    // ) -> (Move, Eval) {

    // }

    fn negamax(
        &mut self,
        board: &Board,
        mut alpha: Eval,
        mut beta: Eval,
        mut depth: u8,
        ply: u8,
    ) -> Eval {
        let mut best_eval: Eval = MIN;
        let mut eval: Eval;
        let in_check = board.king_in_check(self.tables);
        let pv_node: bool = alpha != beta - 1; // False when in a PVS node

        self.nodes += 1;

        if self.is_draw(board) { return 0; } // detect draws
        
        // Mate distance pruning
        alpha = max(-MATE_VALUE + ply as Eval, alpha);
        beta  = min( MATE_VALUE - ply as Eval - 1, beta);
        if alpha >= beta { return alpha; }
        
        // Probe tt for eval and best move
        match self.tt.probe(board.hash) {
            Some(entry) => {
                if entry.get_depth() >= depth {
                    let tt_eval = entry.get_value(ply);
        
                    match entry.get_flag() {
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

                self.history.push(new.hash);
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
                self.history.pop();

                if eval > best_eval {
                    best_eval = eval;
                    best_move = m; // best move is set regardless. facilitates retrieving pv

                    if eval > alpha {               // possible pv node
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
            }
        };
    
        if moves_checked == 0 { // no legal moves
            if in_check {
                best_eval = -MATE_VALUE + ply as Eval  // checkmate
            } else {
                best_eval = 0 // stalemate
            }
        };

        // Insert value in tt
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
        ply: u8,
    ) -> Eval {
        self.nodes += 1;
        let mut best_eval: Eval = evaluate(board);   // try stand pat

        if ply >= MAX_DEPTH { return best_eval };
        
        if best_eval >= beta { return best_eval; };              // beta cutoff
        if best_eval < alpha - FUTILITY_MARGIN { return alpha; } // delta pruning
        if best_eval > alpha { alpha = best_eval; }              // stand pat is pv
    
        let moves: Vec<Move> = self.sorter.sort_captures(board.generate_moves(self.tables));
        for m in moves {
            if let Some(new) = board.make_move(m, self.tables) {
                let eval = - self.quiescence(&new, -beta, -alpha, ply + 1);
                
                best_eval = max(best_eval, eval);
                if eval >= beta { return best_eval; }       // beta cutoff
                else if eval > alpha { alpha = eval; };     // possible pv node
            }
        }

        best_eval // node fails low
    }

    fn is_draw(&self, board: &Board) -> bool {
        board.halfmoves >= 100 || self.is_repetition(board)
    }

    /// Check for repetitions in hash history.
    /// We stop at the first occurrence of the position. We only check 
    fn is_repetition(&self, board: &Board,) -> bool {
        self.history
            .iter()
            .rev()                       // step through history in reverse
            .take(board.halfmoves + 1)   // only check the last "halfmove" elements
            .skip(2)                     // first element is board itself, second is opponent. skip
            .step_by(2)                  // don't check opponent moves
            .any(|&x| { x == board.hash }) // stop at first occurrence
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
        let mut search: Search = Search::new(Vec::new(), &mut tt, &tables);
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
        let mut search: Search = Search::new(Vec::new(), &mut tt, &tables);
        let depth = 10;

        println!("\n --- KILLER POSITION ---\n{}\n\n", board);

        let start = Instant::now();
        let best_move = search.iterative_search(&board, depth);
        let duration = start.elapsed();

        println!("\nDEPTH: {} Found {} in {:?}\n--------------------------------\n", depth, best_move, duration);
    }
}