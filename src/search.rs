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

pub fn search_tree(
    board: &Board,
    tables: &Tables,
    depth: usize,
) -> Move {
    let mut ms: MoveSorter = MoveSorter::new();
    let mut alpha: Eval = MIN;
    let mut best_move: Move = NULL_MOVE;
    let mut nodes: u32 = 0;

    let moves: Vec<Move> = ms.sort_moves(board.generate_moves(tables), 0);

    for m in moves {
        // only consider legal moves
        if let Some(new) = board.make_move(m, tables) {
            let (e, n) = negamax(&new, tables, &mut ms, MIN, -alpha, depth - 1, 1);
            let eval = -e;

            if eval > alpha { 
                best_move = m;
                alpha = eval;
            };
            nodes += n;
        }
    };

    println!("Evaluation: {}", evaluate(board));
    println!("Traversed {} nodes", nodes);
    best_move
}

fn negamax(
    board: &Board,
    tables: &Tables,
    ms: &mut MoveSorter,
    mut alpha: Eval,
    beta: Eval,
    mut depth: usize,
    ply: usize
) -> (Eval, u32) {
    if depth == 0 { return quiescence(board, tables, ms, alpha, beta, ply) }

    let check_flag = board.king_in_check(tables);

    // Extend search depth when king is in check
    if check_flag { depth += 1 }

    let mut nodes: u32 = 1;
    
    let mut legal_count: i32 = 0;
    let moves: Vec<Move> = ms.sort_moves(board.generate_moves(tables), ply);
    
    for m in moves {
        // only consider legal moves
        if let Some(new) = board.make_move(m, tables) {
            let (e, n) = negamax(&new, tables, ms, -beta, -alpha, depth - 1, ply + 1);
            let eval = -e;
            nodes += n;
            
            if eval >= beta { // beta cutoff
                if !(m.is_capture()) {
                    ms.add_killer(m, ply);
                    ms.add_history(m, depth);
                };

                return (beta, nodes);
            }
            if eval > alpha { 
                alpha = eval; 
            }
            legal_count += 1;
        }
    };

    if legal_count == 0 {   // no legal moves
        if check_flag {
            (MATE + ply as Eval, nodes) // checkmate
        } else {
            (0, nodes)      // stalemate
        }
    } else {
        (alpha, nodes)
    }
}

fn quiescence(
    board: &Board,
    tables: &Tables,
    ms: &mut MoveSorter,
    mut alpha: Eval,
    beta: Eval,
    ply: usize
) -> (Eval, u32) {
    let mut eval: Eval = evaluate(board);

    if eval >= beta { return (beta, 1); };  // beta cutoff
    if eval > alpha { alpha = eval; }       // pv node

    let mut nodes = 1;
    let moves: Vec<Move> = ms.sort_captures(board.generate_moves(tables));

    for m in moves {
        if let Some(new) = board.make_move(m, tables) {
            let (e, n) = quiescence(&new, tables, ms, -beta, -alpha, ply + 1);
            eval = -e;
            nodes += n;
            
            if eval >= beta { return (beta, nodes); };  // beta cutoff
            if eval > alpha { alpha = eval; }           // pv ndoe
        }
    }

    (alpha, nodes) // node fails low
}