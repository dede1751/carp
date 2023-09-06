/// Perft is a debugging tool for chess move generators: https://www.chessprogramming.org/Perft
/// Perft can be used in two ways:
///     - Bulk counting: since we generate legal moves, once we are at depth-1 we just return how many
///                    moves we can generate from that position.
///     - Standard: we go all the way down to the leaves
use std::time::Instant;

use crate::board::{Board, QUIETS};

pub const NO_BULK: bool = false;
pub const BULK: bool = true;

impl Board {
    /// Recursive move generation
    fn perft_driver<const BULK: bool>(&self, depth: usize) -> u64 {
        if depth == 0 {
            return 1;
        }

        let move_list = self.gen_moves::<QUIETS>();
        if BULK && depth == 1 {
            return move_list.len() as u64;
        }
        let mut nodes = 0;
        for i in 0..move_list.len() {
            let m = move_list.moves[i];
            let new_board = self.make_move(m);
            nodes += new_board.perft_driver::<BULK>(depth - 1);
        }

        nodes
    }

    /// Cumulative (divide) perft
    pub fn perft<const BULK: bool>(&self, depth: usize) -> u64 {
        let move_list = self.gen_moves::<QUIETS>();
        let mut total_nodes = 0;

        let start = Instant::now();
        for i in 0..move_list.len() {
            let m = move_list.moves[i];
            let start = Instant::now();
            let root = self.make_move(m);
            let nodes = root.perft_driver::<BULK>(depth - 1);
            total_nodes += nodes;
            let duration = start.elapsed();

            println!(
                "{}{} -- {} nodes in {:?}",
                m.get_src(),
                m.get_tgt(),
                nodes,
                duration
            );
        }
        let duration = start.elapsed();

        let perf: u128 = total_nodes as u128 / duration.as_micros();
        println!("\n{total_nodes} nodes in {duration:?} - {perf}Mnodes/s");

        total_nodes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_perft() {
        #[rustfmt::skip]
        const PERFT_SUITE: [(&str, &str, u64, usize); 16] = [
            ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", "Startpos", 119060324, 6),
            ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", "Kiwipete", 193690690, 5),
            ("8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1", "Illegal ep move #1", 1015133, 6),
            ("3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", "Illegal ep move #2", 1134888, 6),
            ("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", "Ep capture checks opponent", 1440467, 6),
            ("5k2/8/8/8/8/8/8/4K2R w K - 0 1", "Short castling gives check", 661072, 6),
            ("3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", "Long castling gives check", 803711, 6),
            ("r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", "Castle rights", 1274206, 4),
            ("r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", "Castling prevented", 1720476, 4),
            ("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", "Promote out of check", 3821001, 6),
            ("8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", "Discovered check", 1004658, 5),
            ("4k3/1P6/8/8/8/8/K7/8 w - - 0 1", "Promote to give check", 217342, 6),
            ("8/P1k5/K7/8/8/8/8/8 w - - 0 1", "Under promote to give check", 92683, 6),
            ("K1k5/8/P7/8/8/8/8/8 w - - 0 1", "Self stalemate", 2217, 6),
            ("8/k1P5/8/1K6/8/8/8/8 w - - 0 1", "Stalemate & checkmate #1", 567584, 7),
            ("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", "Stalemate & checkmate #2", 23527, 4),
        ];

        for (fen, description, correct_count, depth) in PERFT_SUITE {
            let board: Board = fen.parse().unwrap();
            println!("{fen}\n{description}\n{board}");

            let nodes = board.perft::<BULK>(depth);
            assert_eq!(nodes, correct_count);
        }
    }
}
