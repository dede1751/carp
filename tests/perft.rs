//!     Performance testing module.
//! 
//! Run tests with cargo test --release perft -- --show-output

use std::time::Instant;
use carp::{ Board, Tables, MoveList , START_FEN};

const NODE_COUNTS: [u64; 7] = [
    20, 400, 8902, 197281, 4865609, 119060324,  3195901860
];

fn perft_driver(board: &Board, tables: &Tables, depth: u32) -> u64 {
    if depth == 0 { return 1};

    let move_list: MoveList = board.generate_moves(&tables);
    let mut nodes: u64 = 0;
    for m in move_list {
        let new_board = board.make_move(m, tables);
        
        if let Some(b) = new_board { nodes += perft_driver(&b, tables, depth - 1) };
    }

    nodes
}

#[test]
fn default_perft6() {
    let board: Board = Board::from_fen(START_FEN).unwrap();
    let tables: Tables = Tables::new(true);

    println!("\n --- PERFT 1-6 ---");

    for depth in 1..7 {
        let start = Instant::now();
        let nodes = perft_driver(&board, &tables, depth);
        let duration = start.elapsed();

        println!("DEPTH: {} -- {} nodes in {:?}", depth, nodes, duration);

        assert_eq!(nodes, NODE_COUNTS[(depth - 1) as usize]);
    }
}

#[test]
fn cumulative_perft6() {
    let board: Board = Board::from_fen(START_FEN).unwrap();
    let tables: Tables = Tables::new(true);
    let move_list: MoveList = board.generate_moves(&tables);

    println!("\n --- CUMULATIVE PERFT 6 ---");
    for m in move_list {
        let start = Instant::now();
        let root = board.make_move(m, &tables).unwrap();
        let nodes = perft_driver(&root, &tables, 5);
        let duration = start.elapsed();

        println!("{}{} -- {} nodes in {:?}", m.get_src(), m.get_tgt(), nodes, duration);
    }
}