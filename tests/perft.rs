//!     Performance testing module.
//! 
//! Run tests with cargo test --release perft -- --show-output

use std::time::Instant;
use carp::{ Board, Tables, MoveList , START_FEN };

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
fn default_perft7() {
    let board: Board = Board::from_fen(START_FEN).unwrap();
    let tables: Tables = Tables::default();

    println!("\n --- PERFT 1-7 ---");

    for depth in 1..8 {
        let start = Instant::now();
        let nodes = perft_driver(&board, &tables, depth);
        let duration = start.elapsed();

        let perf = nodes as u128 / duration.as_micros();
        println!("DEPTH: {} -- {} nodes in {:?} - {}Mnodes/s", depth, nodes, duration, perf);

        assert_eq!(nodes, NODE_COUNTS[(depth - 1) as usize]);
    }
}

#[test]
fn cumulative_perft6() {
    let board: Board = Board::from_fen(START_FEN).unwrap();
    let tables: Tables = Tables::default();
    let move_list: MoveList = board.generate_moves(&tables);

    println!("\n --- CUMULATIVE PERFT 6 ---");
    let start = Instant::now();
    for m in move_list {
        let start = Instant::now();
        let root = board.make_move(m, &tables).unwrap();
        let nodes = perft_driver(&root, &tables, 5);
        let duration = start.elapsed();

        println!("{}{} -- {} nodes in {:?}", m.get_src(), m.get_tgt(), nodes, duration);
    }
    let duration = start.elapsed();

    println!("\nTotal time: {:?}", duration);
}