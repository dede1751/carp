/// Evaluate board position
/// Uses PeSTO tables with RofChade tuned params and a few extra heuristics
use std::cmp::min;
use std::ops::{AddAssign, Mul, Sub};

use crate::bitboard::*;
use crate::board::*;
use crate::piece::*;
use crate::search::MAX_DEPTH;
use crate::square::*;
use crate::tables::*;

/// Score struct: (midgame_value, endgame_value)
/// Use i32 to avoid internal overflows, then score gets converted to i16 after interpolation
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default, Hash)]
struct S(i32, i32);

impl Mul<i32> for S {
    type Output = S;

    fn mul(self, rhs: i32) -> Self::Output {
        S(self.0 * rhs, self.1 * rhs)
    }
}

impl AddAssign for S {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
        self.1 += rhs.1;
    }
}

impl Sub for S {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        S(self.0 - rhs.0, self.1 - rhs.1)
    }
}

impl S {
    const fn add(self, other: S) -> S {
        S(self.0 + other.0, self.1 + other.1)
    }
}

/// Material values for each piece
#[rustfmt::skip]
const PIECE_VALUES: [S; 6] = [
    S(82, 94), S(337, 281), S(365, 297), S(477, 512), S(1025, 936), S(0, 0)
];

#[rustfmt::skip]
const PIECE_TABLE: [[S; SQUARE_COUNT]; 6] = [
    [ // PAWNS
    S(0,0),      S(0,0),     S(0,0),     S(0,0),     S(0,0),     S(0,0),     S(0,0),     S(0,0),
    S(98,178),   S(134,173), S(61,158),  S(95,134),  S(68,147),  S(126,132), S(34,165),  S(-11,187),
    S(-6,94),    S(7,100),   S(26,85),   S(31,67),   S(65,56),   S(56,53),   S(25,82),   S(-20, 84),
    S(-14,32),   S(13,24),   S(6,13),    S(21,5),    S(23,-2),   S(12,4),    S(17,17),   S(-23,17),
    S(-27,13),   S(-2,9),    S(-5,-3),   S(12,-7),   S(17,-7),   S(6, -8),   S(10, 3),   S(-25,-1),
    S(-26,4),    S(-4,7),    S(-4,-6),   S(-10,1),   S(3,0),     S(3,-5),    S(33,-1),   S(-12,-8),
    S(-35,13),   S(-1,8),    S(-20,8),   S(-23,10),  S(-15,13),  S(24,0),    S(38,2),    S(-22,-7),
    S(0,0),      S(0,0),     S(0,0),     S(0,0),     S(0,0),     S(0,0),     S(0,0),     S(0,0),
    ],
    [ // KNIGHT
    S(-167,-58), S(-89,-38), S(-34,-13), S(-49,-28), S(61,-31),  S(-97,-27), S(-15,-63), S(-107,-99), 
    S(-73,-25),  S(-41,-8),  S(72,-25),  S(36,-2),   S(23,-9),   S(62,-25),  S(7,-24),   S(-17,-52), 
    S(-47,-24),  S(60,-20),  S(37,10),   S(65,9),    S(84,-1),   S(129,-9),  S(73,-19),  S(44,-41), 
    S(-9,-17),   S(17,3),    S(19,22),   S(53,22),   S(37,22),   S(69,11),   S(18,8),    S(22,-18), 
    S(-13,-18),  S(4,-6),    S(16,16),   S(13,25),   S(28,16),   S(19,17),   S(21,4),    S(-8,-18), 
    S(-23,-23),  S(-9,-3),   S(12,-1),   S(10,15),   S(19,10),   S(17,-3),   S(25,-20),  S(-16,-22), 
    S(-29,-42),  S(-53,-20), S(-12,-10), S(-3,-5),   S(-1,-2),   S(18,-20),  S(-14,-23), S(-19,-44), 
    S(-105,-29), S(-21,-51), S(-58,-23), S(-33,-15), S(-17,-22), S(-28,-18), S(-19,-50), S(-23,-64), 
    ],
    [ // BISHOP
    S(-29,-14),  S(4,-21),   S(-82,-11), S(-37,-8),  S(-25,-7),  S(-42,-9),  S(7,-17),   S(-8,-24), 
    S(-26,-8),   S(16,-4),   S(-18,7),   S(-13,-12), S(30,-3),   S(59,-13),  S(18,-4),   S(-47,-14), 
    S(-16,2),    S(37,-8),   S(43,0),    S(40,-1),   S(35,-2),   S(50,6),    S(37,0),    S(-2,4), 
    S(-4,-3),    S(5,9),     S(19,12),   S(50,9),    S(37,14),   S(37,10),   S(7,3),     S(-2,2), 
    S(-6,-6),    S(13,3),    S(13,13),   S(26,19),   S(34,7),    S(12,10),   S(10,-3),   S(4,-9), 
    S(0,-12),    S(15,-3),   S(15,8),    S(15,10),   S(14,13),   S(27,3),    S(18,-7),   S(10,-15), 
    S(4,-14),    S(15,-18),  S(16,-7),   S(0,-1),    S(7,4),     S(21,-9),   S(33,-15),  S(1,-27), 
    S(-33,-23),  S(-3,-9),   S(-14,-23), S(-21,-5),  S(-13,-9),  S(-12,-16), S(-39,-5),  S(-21,-17), 
    ],
    [ // ROOK
    S(32,13),    S(42,10),   S(32,18),   S(51,15),   S(63,12),   S(9,12),    S(31,8),    S(43,5), 
    S(27,11),    S(32,13),   S(58,13),   S(62,11),   S(80,-3),   S(67,3),    S(26,8),    S(44,3), 
    S(-5,7),     S(19,7),    S(26,7),    S(36,5),    S(17,4),    S(45,-3),   S(61,-5),   S(16,-3), 
    S(-24,4),    S(-11,3),   S(7,13),    S(26,1),    S(24,2),    S(35,1),    S(-8,-1),   S(-20,2), 
    S(-36,3),    S(-26,5),   S(-12,8),   S(-1,4),    S(9,-5),    S(-7,-6),   S(6,-8),    S(-23,-11), 
    S(-45,-4),   S(-25,0),   S(-16,-5),  S(-17,-1),  S(3,-7),    S(0,-12),   S(-5,-8),   S(-33,-16), 
    S(-44,-6),   S(-16,-6),  S(-20,0),   S(-9,2),    S(-1,-9),   S(11,-9),   S(-6,-11),  S(-71,-3), 
    S(-19,-9),   S(-13,2),   S(1,3),     S(17,-1),   S(16,-5),   S(7,-13),   S(-37,4),   S(-26,-20), 
    ],
    [ // QUEEN
    S(-28,-9),   S(0,22),    S(29,22),   S(12,27),   S(59,27),   S(44,19),   S(43,10),   S(45,20), 
    S(-24,-17),  S(-39,20),  S(-5,32),   S(1,41),    S(-16,58),  S(57,25),   S(28,30),   S(54,0), 
    S(-13,-20),  S(-17,6),   S(7,9),     S(8,49),    S(29,47),   S(56,35),   S(47,19),   S(57,9), 
    S(-27,3),    S(-27,22),  S(-16,24),  S(-16,45),  S(-1,57),   S(17,40),   S(-2,57),   S(1,36), 
    S(-9,-18),   S(-26,28),  S(-9,19),   S(-10,47),  S(-2,31),   S(-4,34),   S(3,39),    S(-3,23), 
    S(-14,-16),  S(2,-27),   S(-11,15),  S(-2,6),    S(-5,9),    S(2,17),    S(14,10),   S(5,5), 
    S(-35,-22),  S(-8,-23),  S(11,-30),  S(2,-16),   S(8,-16),   S(15,-23),  S(-3,-36),  S(1,-32), 
    S(-1,-33),   S(-18,-28), S(-9,-22),  S(10,-43),  S(-15,-5),  S(-25,-32), S(-31,-20), S(-50,-41), 
    ],
    [ // KING
    S(-65,-74),  S(23,-35),  S(16,-18),  S(-15,-18), S(-56,-11), S(-34,15),  S(2,4),     S(13,-17), 
    S(29,-12),   S(-1,17),   S(-20,14),  S(-7,17),   S(-8,17),   S(-4,38),   S(-38,23),  S(-29,11), 
    S(-9,10),    S(24,17),   S(2,23),    S(-16,15),  S(-20,20),  S(6,45),    S(22,44),   S(-22,13), 
    S(-17,-8),   S(-20,22),  S(-12,24),  S(-27,27),  S(-30,26),  S(-25,33),  S(-14,26),  S(-36,3), 
    S(-49,-18),  S(-1,-4),   S(-27,21),  S(-39,24),  S(-46,27),  S(-44,23),  S(-33,9),   S(-51,-11), 
    S(-14,-19),  S(-14,-3),  S(-22,11),  S(-46,21),  S(-44,23),  S(-30,16),  S(-15,7),   S(-27,-9), 
    S(1,-27),    S(7,-11),   S(-8,4),    S(-64,13),  S(-43,14),  S(-16,4),   S(9,-5),    S(8,-17), 
    S(-15,-53),  S(36,-34),  S(12,-21),  S(-54,-11), S(8,-28),   S(-28,-14), S(24,-24),  S(14,-43), 
]];

/// Setup PST at compile time
const PST: [[S; SQUARE_COUNT]; 12] = {
    let mut tables = [[S(0, 0); SQUARE_COUNT]; 12];
    let mut sq = 0;

    while sq < SQUARE_COUNT {
        let mut p = 0;

        while p < 6 {
            tables[2 * p][sq] = PIECE_VALUES[p].add(PIECE_TABLE[p][sq]);
            tables[2 * p + 1][sq] = PIECE_VALUES[p].add(PIECE_TABLE[p][sq ^ 56]);
            p += 1;
        }
        sq += 1;
    }

    tables
};

/// Pawn evaluation values
const DOUBLED_PAWN: S = S(-5, -10);
const ISOLATED_PAWN: S = S(-5, -10);

#[rustfmt::skip]
const PASSED_PAWN: [[S; RANK_COUNT]; 2] = [
    [ S(0,0), S(70,150), S(40,100), S(25,75), S(10,50), S(5,40), S(5,15), S(0,0) ],
    [ S(0,0), S(5,15), S(5,40), S(10,50), S(25,75), S(40,100), S(70,150), S(0,0) ],
];

/// Rook/King file occupancy values
const SEMI_OPEN_ROOK: S = S(10, 0);
const OPEN_ROOK: S = S(15, 10);

/// King Safety
const KING_SHIELD: S = S(10, 5);
const SEMI_OPEN_KING: S = S(-15, -5);
const OPEN_KING: S = S(-20, 0);

/// Bishop pair and mobility
const BISHOP_PAIR: S = S(30, 35);
const BISHOP_MOBILITY: S = S(3, 5);
const BISHOP_MOBILITY_OFFSET: i32 = 4;

/// Queen mobility
const QUEEN_MOBILITY: S = S(0, 3);
const QUEEN_MOBILITY_OFFSET: i32 = 9;

pub type Eval = i16;
pub const MATE: Eval = 30000;
const LONGEST_MATE: Eval = MATE - MAX_DEPTH as Eval; // Mate lower bound

/// Returns true if the opponent is checkmated
pub fn is_mate(eval: Eval) -> bool {
    eval >= LONGEST_MATE && eval < MATE
}

/// Returns true if the current player is checkmated
pub fn is_mated(eval: Eval) -> bool {
    eval <= -LONGEST_MATE && eval > -MATE
}

/// Returns the piece value according to the game phase (used for futility pruning in QS)
pub fn eval_piece(board: &Board, piece: Piece) -> Eval {
    let score = PIECE_VALUES[piece as usize / 2];
    let game_phase = min(board.game_phase, 24);
    let interpolation = (score.0 * game_phase + score.1 * (24 - game_phase)) / 24;

    interpolation as Eval
}

/// Static position evaluation
/// W is 0 for white and 1 for black (crate::piece::Color cast to usize)
pub fn eval(board: &Board) -> Eval {
    let mut score = S(0, 0);

    score += eval_pawn::<0>(board) - eval_pawn::<1>(board);
    score += eval_knight::<0>(board) - eval_knight::<1>(board);
    score += eval_bishop::<0>(board) - eval_bishop::<1>(board);
    score += eval_rook::<0>(board) - eval_rook::<1>(board);
    score += eval_queen::<0>(board) - eval_queen::<1>(board);
    score += eval_king::<0>(board) - eval_king::<1>(board);

    if board.side == Color::Black {
        score = S(0, 0) - score; // change score to black's perspective
    }

    let game_phase = min(board.game_phase, 24);
    let interpolation = (score.0 * game_phase + score.1 * (24 - game_phase)) / 24;

    interpolation as Eval
}

fn eval_pawn<const W: usize>(board: &Board) -> S {
    let mut score = S(0, 0);
    let own_pawns = board.pieces[WPAWN + W];
    let opp_pawns = board.pieces[BPAWN - W];

    for square in own_pawns {
        // only count the backwards pawn as doubled
        let doubled = (own_pawns & DOUBLED_MASKS[W][square as usize]).count_bits() as i32;
        score += DOUBLED_PAWN * doubled;

        // isolated pawn penalty
        if own_pawns & ISOLATED_MASKS[square as usize] == EMPTY_BB {
            score += ISOLATED_PAWN;
        }

        // passed pawn bonus
        if doubled == 0 && opp_pawns & PASSED_MASKS[W][square as usize] == EMPTY_BB {
            score += PASSED_PAWN[W][square.rank() as usize];
        }

        score += PST[WPAWN + W][square as usize];
    }

    score
}

fn eval_knight<const W: usize>(board: &Board) -> S {
    let mut score = S(0, 0);

    for square in board.pieces[WKNIGHT + W] {
        score += PST[WKNIGHT + W][square as usize];
    }

    score
}

fn eval_bishop<const W: usize>(board: &Board) -> S {
    let mut score = S(0, 0);
    let own_bishops = board.pieces[WBISHOP + W];

    // bishop pair bonus
    if own_bishops.count_bits() >= 2 {
        score += BISHOP_PAIR;
    }

    for square in own_bishops {
        let mobility = bishop_attacks(square, board.occupancy).count_bits() as i32;

        score += BISHOP_MOBILITY * (mobility - BISHOP_MOBILITY_OFFSET);
        score += PST[WBISHOP + W][square as usize];
    }

    score
}

fn eval_rook<const W: usize>(board: &Board) -> S {
    let mut score = S(0, 0);
    let own_rooks = board.pieces[WROOK + W];
    let own_pawns = board.pieces[WPAWN + W];
    let opp_pawns = board.pieces[BPAWN - W];

    for square in own_rooks {
        let file = FILE_MASKS[square as usize];

        // semi open or open file bonus
        if own_pawns & file == EMPTY_BB {
            if opp_pawns & file == EMPTY_BB {
                score += OPEN_ROOK
            } else {
                score += SEMI_OPEN_ROOK
            }
        }

        score += PST[WROOK + W][square as usize];
    }

    score
}

fn eval_queen<const W: usize>(board: &Board) -> S {
    let mut score = S(0, 0);

    for square in board.pieces[WQUEEN + W] {
        let mobility = queen_attacks(square, board.occupancy).count_bits() as i32;

        score += QUEEN_MOBILITY * (mobility - QUEEN_MOBILITY_OFFSET);
        score += PST[WQUEEN + W][square as usize];
    }

    score
}

fn eval_king<const W: usize>(board: &Board) -> S {
    let mut score = S(0, 0);
    let square = board.pieces[WKING + W].lsb();
    let own_pawns = board.pieces[WPAWN + W];
    let opp_pawns = board.pieces[BPAWN - W];
    let file = FILE_MASKS[square as usize];

    // open/semi open file penalty
    if own_pawns & file == EMPTY_BB {
        if opp_pawns & file == EMPTY_BB {
            score += OPEN_KING
        } else {
            score += SEMI_OPEN_KING
        }
    }

    // count shielding pawns around king
    let shields = (own_pawns & king_attacks(square)).count_bits() as i32;

    score += KING_SHIELD * shields;
    score += PST[WKING + W][square as usize];

    score
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symmetry_eval() {
        init_all_tables();
        let b1: Board = "4k3/8/8/3r4/3R4/8/8/4K3 w - - 0 -".parse().unwrap();
        let b2: Board = "4k3/8/8/3r4/3R4/8/8/4K3 b - - 0 -".parse().unwrap();

        println!("{}", b1);

        let eval1 = eval(&b1);
        let eval2 = eval(&b2);

        assert_eq!(eval1, eval2);
        assert_eq!(eval1, 0);
    }

    #[test]
    fn test_overflow() {
        init_all_tables();
        let b: Board = "k7/8/8/8/8/8/8/KQQ5 w KQkq - 0 1".parse().unwrap();
        let eval = eval(&b);

        println!("{}", b);

        assert!(eval > 0);
    }
}
