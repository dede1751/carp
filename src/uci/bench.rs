/// Engine benchmarking
use std::time::Instant;

use crate::{
    board::Board,
    tables::Tables,
    move_order::MoveList,
    moves::*,
};

/// Recursive move generation
fn perft_driver(board: &Board, tables: &Tables, depth: u8) -> u64 {
    if depth == 0 { return 1; }

    let move_list: MoveList = board.generate_moves(tables);
    let mut nodes: u64 = 0;
    for m in move_list {
        let new_board = board.make_move(m, tables);
        
        if let Some(b) = new_board { nodes += perft_driver(&b, tables, depth - 1); }
    }

    nodes
}

/// Cumulative (divide) perft
pub fn perft(board: &Board, tables: &Tables, depth: u8) {
    let move_list: MoveList = board.generate_moves(tables);
    let mut total_nodes = 0;

    let start = Instant::now();
    for m in move_list {
        let start = Instant::now();
        let root = board.make_move(m, tables).unwrap();
        let nodes = perft_driver(&root, tables, depth - 1);
        total_nodes += nodes;
        let duration = start.elapsed();

        println!("{}{} -- {} nodes in {:?}", m.get_src(), m.get_tgt(), nodes, duration);
    }
    let duration = start.elapsed();

    let perf: u128 = total_nodes as u128 / duration.as_micros();
    println!("\n{} nodes in {:?} - {}Mnodes/s", total_nodes, duration, perf);
}

/// Benchmark engine using list of EPD
/// 
/// Iterate over the bencher, search the position and check the results against best/avoid move
pub struct Bencher<'a> {
    counter: usize,
    score: u32,
    best_move: bool,
    moves: Vec<Move>,
    tables: &'a Tables
}

/// Iterates over the test boards
impl<'a> Iterator for Bencher<'a> {
    type Item = Board;

    fn next(&mut self) -> Option<Self::Item> {
        if self.counter < EIGENMANN_RAPID.len() {
            let epd = read_epd(EIGENMANN_RAPID[self.counter], self.tables);
            self.counter += 1;
            self.best_move = epd.1;
            self.moves = epd.2;

            println!("\n{}", epd.3);
            println!("{}", epd.0);

            Some(epd.0)
        } else {
            println!("\nScored: {}/{}", self.score, self.counter);
            None
        }
    }
}

impl<'a> Bencher<'a> {
    pub fn new(tables: &'a Tables) -> Bencher<'a> {
        Bencher {
            counter: 0,
            score: 0,
            best_move: false,
            moves: Vec::new(),
            tables,
        }
    }

    /// Checks if the move matches the epd best/avoid move
    pub fn submit_move(&mut self, m: &Move) {
        println!("\nMove: {}", m);

        if self.best_move { print!("Best moves: "); }
        else { print!("Avoid moves: "); }

        for m in &self.moves {
            print!("{} ", m);
        }
        
        if  ( self.best_move &&  self.moves.contains(m))   ||
            (!self.best_move && !self.moves.contains(m)) {
            self.score += 1;
            println!("\nCORRECT!");
        } else {
            println!("\nINCORRECT!");
        }
    }
}



fn read_epd(epd: &str, tables: &Tables) -> (Board, bool, Vec<Move>, String) {
    let mut partial_fen: String = epd.clone()
        .split_whitespace()
        .take(4)
        .collect::<Vec<&str>>()
        .join(" ");
        
    let mut epd_commands =  epd.trim_start_matches(&partial_fen).split(";");
    partial_fen.push_str(" 0 -");

    let board = Board::try_from(&partial_fen[..]).unwrap();
    let mut moves: Vec<Move> = Vec::new();
    let mut best_move = true;
    let mut id = String::new();

    while let Some(command) = epd_commands.next() {
        if command == "" { break; }
        let mut tokens = command.trim().split_whitespace();

        let opcode = tokens.next().unwrap();
        match opcode {
            "bm" | "am" => {
                if opcode == "am" { best_move = false; }
                                
                for t in tokens {
                    let tgt_move = t.trim_end_matches(",");
                    let m = board.generate_moves(tables)
                        .into_iter()
                        .find(|m|
                            board.make_move(*m, tables).is_some()       &&
                            tgt_move == m.to_algebraic(&board, tables)
                                         .trim_end_matches(|x| x == '#' || x == '+'))
                        .unwrap();
                    moves.push(m);
                }
            }

            "id" => id = tokens.collect(),
            _ => {}
        }
    }
        
    (board, best_move, moves, id)
}

pub const EIGENMANN_RAPID: [&str; 111] = [
    r#"r1bqk1r1/1p1p1n2/p1n2pN1/2p1b2Q/2P1Pp2/1PN5/PB4PP/R4RK1 w q - bm Rxf4; id "ERET 001 - Relief";"#,
    r#"r1n2N1k/2n2K1p/3pp3/5Pp1/b5R1/8/1PPP4/8 w - - bm Ng6; id "ERET 002 - Zugzwang";"#,
    r#"r1b1r1k1/1pqn1pbp/p2pp1p1/P7/1n1NPP1Q/2NBBR2/1PP3PP/R6K w - - bm f5; id "ERET 003 - Open Line";"#,
    r#"5b2/p2k1p2/P3pP1p/n2pP1p1/1p1P2P1/1P1KBN2/7P/8 w - - bm Nxg5; id "ERET 004 - Endgame";"#,
    r#"r3kbnr/1b3ppp/pqn5/1pp1P3/3p4/1BN2N2/PP2QPPP/R1BR2K1 w kq - bm Bxf7; id "ERET 005 - Bishop Sacrifice f7";"#,
    r#"r2r2k1/1p1n1pp1/4pnp1/8/PpBRqP2/1Q2B1P1/1P5P/R5K1 b - - bm Nc5; id "ERET 006 - Knight Sacrifice";"#,
    r#"2rq1rk1/pb1n1ppN/4p3/1pb5/3P1Pn1/P1N5/1PQ1B1PP/R1B2RK1 b - - bm Nde5; id "ERET 007 - Bishop Pair";"#,
    r#"r2qk2r/ppp1bppp/2n5/3p1b2/3P1Bn1/1QN1P3/PP3P1P/R3KBNR w KQkq - bm Qxd5; id "ERET 008 - Center";"#,
    r#"rnb1kb1r/p4p2/1qp1pn2/1p2N2p/2p1P1p1/2N3B1/PPQ1BPPP/3RK2R w Kkq - bm Ng6; id "ERET 009 - Knight Sacrifice";"#,
    r#"5rk1/pp1b4/4pqp1/2Ppb2p/1P2p3/4Q2P/P3BPP1/1R3R1K b - - bm d4; id "ERET 010 - Passed Pawn";"#,
    r#"r1b2r1k/ppp2ppp/8/4p3/2BPQ3/P3P1K1/1B3PPP/n3q1NR w - - bm dxe5, Nf3; id "ERET 011 - Attacking Castle";"#,
    r#"1nkr1b1r/5p2/1q2p2p/1ppbP1p1/2pP4/2N3B1/1P1QBPPP/R4RK1 w - - bm Nxd5; id "ERET 012 - Relief";"#,
    r#"1nrq1rk1/p4pp1/bp2pn1p/3p4/2PP1B2/P1PB2N1/4QPPP/1R2R1K1 w - - bm Qd2, Bc2; id "ERET 013 - Center";"#,
    r#"5k2/1rn2p2/3pb1p1/7p/p3PP2/PnNBK2P/3N2P1/1R6 w - - bm Nf3; id "ERET 014 - Endgame";"#,
    r#"8/p2p4/r7/1k6/8/pK5Q/P7/b7 w - - bm Qd3; id "ERET 015 - Endgame";"#,
    r#"1b1rr1k1/pp1q1pp1/8/NP1p1b1p/1B1Pp1n1/PQR1P1P1/4BP1P/5RK1 w - - bm Nc6; id "ERET 016 - Pos. Sacrifice";"#,
    r#"1r3rk1/6p1/p1pb1qPp/3p4/4nPR1/2N4Q/PPP4P/2K1BR2 b - - bm Rxb2; id "ERET 017 - King Attack";"#,
    r#"r1b1kb1r/1p1n1p2/p3pP1p/q7/3N3p/2N5/P1PQB1PP/1R3R1K b kq - bm Qg5; id "ERET 018 - Development";"#,
    r#"3kB3/5K2/7p/3p4/3pn3/4NN2/8/1b4B1 w - - bm Nf5; id "ERET 019 - Endgame";"#,
    r#"1nrrb1k1/1qn1bppp/pp2p3/3pP3/N2P3P/1P1B1NP1/PBR1QPK1/2R5 w - - bm Bxh7; id "ERET 020 - Bishop Sacrifice h7";"#,
    r#"3rr1k1/1pq2b1p/2pp2p1/4bp2/pPPN4/4P1PP/P1QR1PB1/1R4K1 b - - bm Rc8; id "ERET 021 - Prophylaxis";"#,
    r#"r4rk1/p2nbpp1/2p2np1/q7/Np1PPB2/8/PPQ1N1PP/1K1R3R w - - bm h4; id "ERET 022 - Passed Pawn";"#,
    r#"r3r2k/1bq1nppp/p2b4/1pn1p2P/2p1P1QN/2P1N1P1/PPBB1P1R/2KR4 w - - bm Ng6; id "ERET 023 - Attacking Castle";"#,
    r#"r2q1r1k/3bppbp/pp1p4/2pPn1Bp/P1P1P2P/2N2P2/1P1Q2P1/R3KB1R w KQ - am b3; id "ERET 024 - Development";"#,
    r#"2kb4/p7/r1p3p1/p1P2pBp/R2P3P/2K3P1/5P2/8 w - - bm Bxd8; id "ERET 025 - Endgame";"#,
    r#"rqn2rk1/pp2b2p/2n2pp1/1N2p3/5P1N/1PP1B3/4Q1PP/R4RK1 w - - bm Nxg6; id "ERET 026 - Knight Sacrifice";"#,
    r#"8/3Pk1p1/1p2P1K1/1P1Bb3/7p/7P/6P1/8 w - - bm g4; id "ERET 027 - Zugzwang";"#,
    r#"4rrk1/Rpp3pp/6q1/2PPn3/4p3/2N5/1P2QPPP/5RK1 w - - am Rxb7; id "ERET 028 - Poisoned Pawn";"#,
    r#"2q2rk1/2p2pb1/PpP1p1pp/2n5/5B1P/3Q2P1/4PPN1/2R3K1 w - - bm Rxc5; id "ERET 029 - Exchange Sacrifice";"#,
    r#"rnbq1r1k/4p1bP/p3p3/1pn5/8/2Np1N2/PPQ2PP1/R1B1KB1R w KQ - bm Nh4; id "ERET 030 - Initiative";"#,
    r#"4b1k1/1p3p2/4pPp1/p2pP1P1/P2P4/1P1B4/8/2K5 w - - bm b4; id "ERET 031 - Endgame";"#,
    r#"8/7p/5P1k/1p5P/5p2/2p1p3/P1P1P1P1/1K3Nb1 w - - bm Ng3; id "ERET 032 - Zugzwang";"#,
    r#"r3kb1r/ppnq2pp/2n5/4pp2/1P1PN3/P4N2/4QPPP/R1B1K2R w KQkq - bm Nxe5; id "ERET 033 - Initiative";"#,
    r#"b4r1k/6bp/3q1ppN/1p2p3/3nP1Q1/3BB2P/1P3PP1/2R3K1 w - - bm Rc8; id "ERET 034 - Bishop Pair";"#,
    r#"r3k2r/5ppp/3pbb2/qp1Np3/2BnP3/N7/PP1Q1PPP/R3K2R w KQkq - bm Nxb5; id "ERET 035 - Exchange Sacrifice";"#,
    r#"r1k1n2n/8/pP6/5R2/8/1b1B4/4N3/1K5N w - - bm b7; id "ERET 036 - Endgame";"#,
    r#"1k6/bPN2pp1/Pp2p3/p1p5/2pn4/3P4/PPR5/1K6 w - - bm Na8; id "ERET 037 - Zugzwang";"#,
    r#"8/6N1/3kNKp1/3p4/4P3/p7/P6b/8 w - - bm exd5; id "ERET 038 - Endgame";"#,
    r#"r1b1k2r/pp3ppp/1qn1p3/2bn4/8/6P1/PPN1PPBP/RNBQ1RK1 w kq - bm a3; id "ERET 039 - Development";"#,
    r#"r3kb1r/3n1ppp/p3p3/1p1pP2P/P3PBP1/4P3/1q2B3/R2Q1K1R b kq - bm Bc5; id "ERET 040 - King Safety";"#,
    r#"3q1rk1/2nbppb1/pr1p1n1p/2pP1Pp1/2P1P2Q/2N2N2/1P2B1PP/R1B2RK1 w - - bm Nxg5; - id "ERET 041 - Knight Sacrifice";"#,
    r#"8/2k5/N3p1p1/2KpP1P1/b2P4/8/8/8 b - - bm Kb7; id "ERET 042 - Endgame";"#,
    r#"2r1rbk1/1pqb1p1p/p2p1np1/P4p2/3NP1P1/2NP1R1Q/1P5P/R5BK w - - bm Nxf5; id "ERET 043 - Knight Sacrifice";"#,
    r#"rnb2rk1/pp2q2p/3p4/2pP2p1/2P1Pp2/2N5/PP1QBRPP/R5K1 w - - bm h4; id "ERET 044 - Open Line";"#,
    r#"5rk1/p1p1rpb1/q1Pp2p1/3Pp2p/4Pn2/1R4N1/P1BQ1PPP/R5K1 w - - bm Rb4; id "ERET 045 - Initiative";"#,
    r#"8/4nk2/1p3p2/1r1p2pp/1P1R1N1P/6P1/3KPP2/8 w - - bm Nd3; id "ERET 046 - Endgame";"#,
    r#"4kbr1/1b1nqp2/2p1p3/2N4p/1p1PP1pP/1PpQ2B1/4BPP1/r4RK1 w - - bm Nxb7; id "ERET 047 - Relief";"#,
    r#"r1b2rk1/p2nqppp/1ppbpn2/3p4/2P5/1PN1PN2/PBQPBPPP/R4RK1 w - - bm cxd5; id "ERET 048 - Stong Squares";"#,
    r#"r1b1kq1r/1p1n2bp/p2p2p1/3PppB1/Q1P1N3/8/PP2BPPP/R4RK1 w kq - bm f4; id "ERET 049 - Development";"#,
    r#"r4r1k/p1p3bp/2pp2p1/4nb2/N1P4q/1P5P/PBNQ1PP1/R4RK1 b - - bm Nf3; id "ERET 050 - King Attack";"#,
    r#"6k1/pb1r1qbp/3p1p2/2p2p2/2P1rN2/1P1R3P/PB3QP1/3R2K1 b - - bm Bh6; id "ERET 051 - Defence";"#,
    r#"2r2r2/1p1qbkpp/p2ppn2/P1n1p3/4P3/2N1BB2/QPP2PPP/R4RK1 w - - bm b4; id "ERET 052 - Stong Squares";"#,
    r#"r1bq1rk1/p4ppp/3p2n1/1PpPp2n/4P2P/P1PB1PP1/2Q1N3/R1B1K2R b KQ - bm c4; id "ERET 053 - Pos. Sacrifice";"#,
    r#"2b1r3/5pkp/6p1/4P3/QppqPP2/5RPP/6BK/8 b - - bm c3; id "ERET 054 - Endgame";"#,
    r#"r2q1rk1/1p2bpp1/p1b2n1p/8/5B2/2NB4/PP1Q1PPP/3R1RK1 w - - bm Bxh6; id "ERET 055 - Bishop Sacrifice h6";"#,
    r#"r2qr1k1/pp2bpp1/2pp3p/4nbN1/2P4P/4BP2/PPPQ2P1/1K1R1B1R w - - bm Be2; id "ERET 056 - Zwischenzug";"#,
    r#"r2qr1k1/pp1bbp2/n5p1/2pPp2p/8/P2PP1PP/1P2N1BK/R1BQ1R2 w - - bm d6; id "ERET 057 - Exchange";"#,
    r#"8/8/R7/1b4k1/5p2/1B3r2/7P/7K w - - bm h4; id "ERET 058 - Endgame";"#,
    r#"rq6/5k2/p3pP1p/3p2p1/6PP/1PB1Q3/2P5/1K6 w - - bm Qd3; id "ERET 059 - Endgame";"#,
    r#"q2B2k1/pb4bp/4p1p1/2p1N3/2PnpP2/PP3B2/6PP/2RQ2K1 b - - bm Qxd8; id "ERET 060 - King Attack";"#,
    r#"4rrk1/pp4pp/3p4/3P3b/2PpPp1q/1Q5P/PB4B1/R4RK1 b - - bm Rf6; id "ERET 061 - King Attack";"#,
    r#"rr1nb1k1/2q1b1pp/pn1p1p2/1p1PpNPP/4P3/1PP1BN2/2B2P2/R2QR1K1 w - - bm g6; id "ERET 062 - Stong Squares";"#,
    r#"r3k2r/4qn2/p1p1b2p/6pB/P1p5/2P5/5PPP/RQ2R1K1 b kq - bm Kf8; id "ERET 063 - Defence";"#,
    r#"8/1pp5/p3k1pp/8/P1p2PPP/2P2K2/1P3R2/5r2 b - - am Rxf2; id "ERET 064 - Endgame";"#,
    r#"1r3rk1/2qbppbp/3p1np1/nP1P2B1/2p2P2/2N1P2P/1P1NB1P1/R2Q1RK1 b - - bm Qb6; id "ERET 065 - Zwischenzug";"#,
    r#"8/2pN1k2/p4p1p/Pn1R4/3b4/6Pp/1P3K1P/8 w - - bm Ke1; id "ERET 066 - Endgame";"#,
    r#"5r1k/1p4bp/3p1q2/1NpP1b2/1pP2p2/1Q5P/1P1KBP2/r2RN2R b - - bm f3; id "ERET 067 - Clearance";"#,
    r#"r3kb1r/pbq2ppp/1pn1p3/2p1P3/1nP5/1P3NP1/PB1N1PBP/R2Q1RK1 w kq - bm a3; id "ERET 068 - Open Line";"#,
    r#"5rk1/n2qbpp1/pp2p1p1/3pP1P1/PP1P3P/2rNPN2/R7/1Q3RK1 w - - bm h5; id "ERET 069 - King Attack";"#,
    r#"r5k1/1bqp1rpp/p1n1p3/1p4p1/1b2PP2/2NBB1P1/PPPQ4/2KR3R w - - bm a3; id "ERET 070 - Stong Squares";"#,
    r#"1r4k1/1nq3pp/pp1pp1r1/8/PPP2P2/6P1/5N1P/2RQR1K1 w - - bm f5; id "ERET 071 - Deflection";"#,
    r#"q5k1/p2p2bp/1p1p2r1/2p1np2/6p1/1PP2PP1/P2PQ1KP/4R1NR b - - bm Qd5; id "ERET 072 - Centralization";"#,
    r#"r4rk1/ppp2ppp/1nnb4/8/1P1P3q/PBN1B2P/4bPP1/R2QR1K1 w - - bm Qxe2; id "ERET 073 - Mobility";"#,
    r#"1r3k2/2N2pp1/1pR2n1p/4p3/8/1P1K1P2/P5PP/8 w - - bm Kc4; id "ERET 074 - Endgame";"#,
    r#"6r1/6r1/2p1k1pp/p1pbP2q/Pp1p1PpP/1P1P2NR/1KPQ3R/8 b - - bm Qf5; id "ERET 075 - Fortress";"#,
    r#"r1b1kb1r/1p1npppp/p2p1n2/6B1/3NPP2/q1N5/P1PQ2PP/1R2KB1R w Kkq - bm Bxf6; id "ERET 076 - Development";"#,
    r#"r3r1k1/1bq2ppp/p1p2n2/3ppPP1/4P3/1PbB4/PBP1Q2P/R4R1K w - - bm gxf6; id "ERET 077 - Attacking Castle";"#,
    r#"r4rk1/ppq3pp/2p1Pn2/4p1Q1/8/2N5/PP4PP/2KR1R2 w - - bm Rxf6; id "ERET 078 - Passed Pawn";"#,
    r#"r1bqr1k1/3n1ppp/p2p1b2/3N1PP1/1p1B1P2/1P6/1PP1Q2P/2KR2R1 w - - bm Qxe8; id "ERET 079 - Queen Sacrifice";"#,
    r#"5rk1/1ppbq1pp/3p3r/pP1PppbB/2P5/P1BP4/5PPP/3QRRK1 b - - bm Bc1; id "ERET 080 - Clearance";"#,
    r#"r3r1kb/p2bp2p/1q1p1npB/5NQ1/2p1P1P1/2N2P2/PPP5/2KR3R w - - bm Bg7; id "ERET 081 - King Attack";"#,
    r#"8/3P4/1p3b1p/p7/P7/1P3NPP/4p1K1/3k4 w - - bm g4; id "ERET 082 - Endgame";"#,
    r#"3q1rk1/7p/rp1n4/p1pPbp2/P1P2pb1/1QN4P/1B2B1P1/1R3RK1 w - - bm Nb5; id "ERET 083 - Exchange";"#,
    r#"4r1k1/1r1np3/1pqp1ppB/p7/2b1P1PQ/2P2P2/P3B2R/3R2K1 w - - bm Bg7 Bg5; id "ERET 084 - King Attack";"#,
    r#"r4rk1/q4bb1/p1R4p/3pN1p1/8/2N3P1/P4PP1/3QR1K1 w - - bm Ng4; id "ERET 085 - Exchange";"#,
    r#"r3k2r/pp2pp1p/8/q2Pb3/2P5/4p3/B1Q2PPP/2R2RK1 w kq - bm c5; id "ERET 086 - Exchange Sacrifice";"#,
    r#"r3r1k1/1bnq1pbn/p2p2p1/1p1P3p/2p1PP1B/P1N2B1P/1PQN2P1/3RR1K1 w - - bm e5; id "ERET 087 - Clearance";"#,
    r#"8/4k3/p2p2p1/P1pPn2p/1pP1P2P/1P1NK1P1/8/8 w - - bm g4; id "ERET 088 - Endgame";"#,
    r#"8/2P1P3/b1B2p2/1pPRp3/2k3P1/P4pK1/nP3p1p/N7 w - - bm e8N; id "ERET 089 - Underpromotion";"#,
    r#"4K1k1/8/1p5p/1Pp3b1/8/1P3P2/P1B2P2/8 w - - bm f4; id "ERET 090 - Endgame";"#,
    r#"8/6p1/3k4/3p1p1p/p2K1P1P/4P1P1/P7/8 b - - bm g6, Kc6; id "ERET 091 - Endgame";"#,
    r#"r1b2rk1/ppp3p1/4p2p/4Qpq1/3P4/2PB4/PPK2PPP/R6R b - - am Qxg2; id "ERET 092 - Poisoned Pawn";"#,
    r#"2b1r3/r2ppN2/8/1p1p1k2/pP1P4/2P3R1/PP3PP1/2K5 w - - bm Nd6; id "ERET 093 - Endgame";"#,
    r#"2k2Br1/p6b/Pq1r4/1p2p1b1/1Ppp2p1/Q1P3N1/5RPP/R3N1K1 b - - bm Rf6; id "ERET 094 - Queen Sacrifice";"#,
    r#"r2qk2r/ppp1b1pp/2n1p3/3pP1n1/3P2b1/2PB1NN1/PP4PP/R1BQK2R w KQkq - bm Nxg5; id "ERET 095 - Queen Sacrifice";"#,
    r#"8/8/4p1Pk/1rp1K1p1/4P1P1/1nP2Q2/p2b1P2/8 w - - bm Kf6; id "ERET 096 - Endgame";"#,
    r#"2k5/p7/Pp1p1b2/1P1P1p2/2P2P1p/3K3P/5B2/8 w - - bm c5; id "ERET 097 - Endgame";"#,
    r#"8/6pp/5k2/1p1r4/4R3/7P/5PP1/5K2 w - - am Ke2; id "ERET 098 - Endgame";"#,
    r#"3q1r1k/4RPp1/p6p/2pn4/2P5/1P6/P3Q2P/6K1 w - - bm Re8; id "ERET 099 - Endgame";"#,
    r#"rn2k2r/3pbppp/p3p3/8/Nq1Nn3/4B1P1/PP3P1P/R2Q1RK1 w k - bm Nf5; id "ERET 100 - Initiative";"#,
    r#"r1b1kb1N/pppnq1pB/8/3p4/3P4/8/PPPK1nPP/RNB1R3 b q - bm Ne5; id "ERET 101 - Development";"#,
    r#"N4rk1/pp1b1ppp/n3p1n1/3pP1Q1/1P1N4/8/1PP2PPP/q1B1KB1R b K - bm Nxb4; id "ERET 102 - King Attack";"#,
    r#"4k1br/1K1p1n1r/2p2pN1/P2p1N2/2P3pP/5B2/P2P4/8 w - - bm Kc8; id "ERET 103 - Zugzwang";"#,
    r#"r1bqkb1r/ppp3pp/2np4/3N1p2/3pnB2/5N2/PPP1QPPP/2KR1B1R b kq - bm Ne7; id "ERET 104 - Development";"#,
    r#"r3kb1r/pbqp1pp1/1pn1pn1p/8/3PP3/2PB1N2/3N1PPP/R1BQR1K1 w kq - bm e5; id "ERET 105 - Stong Squares";"#,
    r#"r2r2k1/pq2bppp/1np1bN2/1p2B1P1/5Q2/P4P2/1PP4P/2KR1B1R b - - bm Bxf6; id "ERET 106 - King Safety";"#,
    r#"1r1r2k1/2pq3p/4p3/2Q1Pp2/1PNn1R2/P5P1/5P1P/4R2K b - - bm Rb5; id "ERET 107 - Defence";"#,
    r#"8/5p1p/3P1k2/p1P2n2/3rp3/1B6/P4R2/6K1 w - - bm Ba4; id "ERET 108 - Endgame";"#,
    r#"2rbrnk1/1b3p2/p2pp3/1p4PQ/1PqBPP2/P1NR4/2P4P/5RK1 b - - bm Qxd4; id "ERET 109 - Relief";"#,
    r#"4r1k1/1bq2r1p/p2p1np1/3Pppb1/P1P5/1N3P2/1R2B1PP/1Q1R2BK w - - bm c5; id "ERET 110 - Passed Pawn";"#,
    r#"8/8/8/8/4kp2/1R6/P2q1PPK/8 w - - bm a3; id "ERET 111 - Fortress";"#,
];