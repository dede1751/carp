//! # Search node count testing module.
//! 
//! Run tests with cargo test --release search -- --show-output

use std::time::Instant;
use carp::{ Board, Tables, Search, KILLER_FEN, TRICKY_FEN };

#[test]
fn search_tricky10() {
    let board: Board = Board::from_fen(TRICKY_FEN).unwrap();
    let tables: Tables = Tables::default();
    let mut search: Search = Search::new(&tables);
    let depth = 10;

    println!("\n --- TRICKY POSITION ---\n{}\n\n", board);

    let start = Instant::now();
    let best_move = search.iterative_search(&board, depth);
    let duration = start.elapsed();

    println!("\nDEPTH: {} Found {} in {:?}\n--------------------------------\n", depth, best_move, duration);
}

#[test]
fn search_killer10() {
    let board: Board = Board::from_fen(KILLER_FEN).unwrap();
    let tables: Tables = Tables::default();
    let mut search: Search = Search::new(&tables);
    let depth = 10;

    println!("\n --- KILLER POSITION ---\n{}\n\n", board);

    let start = Instant::now();
    let best_move = search.iterative_search(&board, depth);
    let duration = start.elapsed();

    println!("\nDEPTH: {} Found {} in {:?}\n--------------------------------\n", depth, best_move, duration);
}