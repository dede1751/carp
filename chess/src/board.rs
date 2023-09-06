use std::{fmt, str::FromStr};

use crate::{
    bitboard::BitBoard,
    castle::CastlingRights,
    moves::{Move, MoveType},
    params::*,
    piece::{Color, Piece},
    square::{File, Rank, Square},
    zobrist::ZHash,
};

// Re-export the movegen module into the board.
pub use crate::movegen::{gen::*, make_move::*, perft::*};

/// Bitboard-based board representation
/// Any board without a king for each player (and with more than one for either) is UB!
#[derive(Clone, Debug)]
pub struct Board {
    // Main bitboards
    piece_bb: [BitBoard; Piece::COUNT],
    side_bb: [BitBoard; 2],

    // Piece map for piece_at lookup
    piece: [Option<Piece>; Square::COUNT],

    // Other positional information
    pub side: Color,
    pub castling_rights: CastlingRights,
    pub en_passant: Option<Square>,
    pub halfmoves: usize,
    pub hash: ZHash,

    // Checkers kept for in_check() within search
    pub checkers: BitBoard,
}

/// Pretty print board state
impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board_str = format!(
            "\n FEN: {}\n\n\t┏━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┳━━━┓",
            self.to_fen()
        );

        for rank in Rank::ALL {
            board_str.push_str(format!("\n      {} ┃ ", 8 - rank as usize).as_str());

            for file in File::ALL {
                let square = Square::from_coords(file, rank);
                let piece_str =
                    self.piece[square as usize].map_or(String::from(" "), |p| p.to_string());

                board_str.push_str(&piece_str);
                board_str.push_str(" ┃ ");
            }
            if rank != Rank::First {
                board_str.push_str("\n\t┣━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━╋━━━┫");
            }
        }
        board_str
            .push_str("\n\t┗━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┻━━━┛\n\t  A   B   C   D   E   F   G   H\n");

        let en_passant_str = match self.en_passant {
            Some(square) => format!("{square}"),
            None => String::from("-"),
        };

        write!(
            f,
            "{board_str}
 Side to move      : {}
 Castling Rights   : {}
 En Passant Square : {en_passant_str}
 Halfmoves         : {}
 ",
            self.side, self.castling_rights, self.halfmoves,
        )
    }
}

/// Init board state from FEN string with complete error handling (no legality check)
impl FromStr for Board {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let fen: Vec<&str> = s.split_whitespace().take(6).collect();
        if fen.len() != 6 {
            return Err("Invalid fen!");
        }

        let mut board = Self::new();
        let board_str = fen[0];
        let mut token_count = 0; // used for checking that number of tokens is correct

        let (mut file, mut rank) = (File::A, Rank::Eight);
        for token in board_str.chars() {
            match token {
                '/' => {
                    if token_count != 8 {
                        return Err("Invalid fen!");
                    };

                    rank = rank.down();
                    token_count = 0;
                }
                '1'..='8' => {
                    for _ in '1'..=token {
                        file = file.right();
                        token_count += 1;
                    }
                }
                _ => {
                    board.set_piece(Piece::try_from(token)?, Square::from_coords(file, rank));
                    file = file.right();
                    token_count += 1;
                }
            }
        }

        if token_count != 8 {
            return Err("Invalid fen!");
        }

        match fen[1] {
            "w" => {
                board.side = Color::White;
                board.hash.toggle_side();
            }
            "b" => board.side = Color::Black,
            _ => return Err("Invalid fen!"),
        }

        let rights: CastlingRights = fen[2].parse()?;
        board.castling_rights = rights;
        board.hash.toggle_castle(rights);

        match fen[3] {
            "-" => board.en_passant = None,
            _ => {
                let ep_square: Square = fen[3].parse()?;

                board.en_passant = Some(ep_square);
                board.hash.toggle_ep(ep_square);
            }
        }

        match fen[4].parse::<usize>() {
            Ok(hm) => board.halfmoves = hm,
            Err(_) => return Err("Invalid halfmove count!"),
        }

        board.checkers = board.checkers();

        Ok(board)
    }
}

/// Convert board state to FEN string
impl Board {
    pub fn to_fen(&self) -> String {
        let mut fen = String::new();

        for rank in Rank::ALL {
            let mut empty = 0;

            for file in File::ALL {
                let square = Square::from_coords(file, rank);

                if let Some(p) = self.piece[square as usize] {
                    if empty > 0 {
                        fen.push_str(&empty.to_string());
                        empty = 0;
                    }
                    fen.push(p.to_char())
                } else {
                    empty += 1;
                }
            }

            if empty > 0 {
                fen.push_str(&empty.to_string());
            }
            if rank != Rank::First {
                fen.push('/');
            }
        }

        match self.side {
            Color::White => fen.push_str(" w "),
            Color::Black => fen.push_str(" b "),
        }

        fen.push_str(&self.castling_rights.to_string());

        if let Some(square) = self.en_passant {
            fen.push_str(&format!(" {square}"));
        } else {
            fen.push_str(" -");
        }

        fen.push_str(&format!(" {} 1", self.halfmoves));

        fen
    }
}

/// Default to starting position
impl Default for Board {
    fn default() -> Self {
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            .parse()
            .unwrap()
    }
}

/// Implement all piece bitboard lookups (ugly because concatenating idents seems impossible)
macro_rules! impl_piece_lookups {
    ($($piece:expr, $own:ident, $opp:ident, $tot:ident),*) => {
        $(impl Board {
            pub const fn $own(&self) -> BitBoard {
                BitBoard(self.piece_bb[$piece].0 & self.side_bb[self.side as usize].0)
            }
            pub const fn $opp(&self) -> BitBoard {
                BitBoard(self.piece_bb[$piece].0 & self.side_bb[self.side as usize ^ 1].0)
            }
            pub const fn $tot(&self) -> BitBoard {
                self.piece_bb[$piece]
            }
        })*
    };
}
impl_piece_lookups! {
    0, own_pawns, opp_pawns, pawns,
    1, own_knights, opp_knights, knights,
    2, own_bishops, opp_bishops, bishops,
    3, own_rooks, opp_rooks, rooks,
    4, own_queens, opp_queens, queens,
    5, own_king, opp_king, kings
}

/// Implement side occupancy and diagonal/hv slider lookups
impl Board {
    pub const fn white(&self) -> BitBoard {
        self.side_bb[Color::White as usize]
    }
    pub const fn black(&self) -> BitBoard {
        self.side_bb[Color::Black as usize]
    }
    pub const fn occupancy(&self) -> BitBoard {
        BitBoard(self.white().0 | self.black().0)
    }
    pub const fn piece_occupancy(&self, piece: Piece) -> BitBoard {
        BitBoard(self.piece_bb[piece.index()].0 & self.side_bb[piece.color() as usize].0)
    }
    pub const fn own_occupancy(&self) -> BitBoard {
        self.side_bb[self.side as usize]
    }
    pub const fn opp_occupancy(&self) -> BitBoard {
        self.side_bb[self.side as usize ^ 1]
    }
    pub const fn opp_queen_bishop(&self) -> BitBoard {
        BitBoard(self.opp_queens().0 | self.opp_bishops().0)
    }
    pub const fn opp_queen_rook(&self) -> BitBoard {
        BitBoard(self.opp_queens().0 | self.opp_rooks().0)
    }
}

/// Implement board modification
impl Board {
    /// Initialize an empty board
    pub const fn new() -> Self {
        Self {
            piece_bb: [BitBoard::EMPTY; Piece::COUNT],
            side_bb: [BitBoard::EMPTY; 2],

            piece: [None; Square::COUNT],

            side: Color::White,
            castling_rights: CastlingRights::NONE,
            en_passant: None,
            halfmoves: 0,
            hash: ZHash::NULL,

            checkers: BitBoard::EMPTY,
        }
    }

    /// Set the piece on the board at the given square (remove first, set later)
    pub(crate) fn set_piece(&mut self, piece: Piece, square: Square) {
        let p = piece.index();
        let c = piece.color() as usize;

        self.piece_bb[p] = self.piece_bb[p].set_bit(square);
        self.side_bb[c] = self.side_bb[c].set_bit(square);
        self.piece[square as usize] = Some(piece);
        self.hash.toggle_piece(piece, square);
    }

    /// Remove the piece at the given square on the board (set first, remove later)
    /// The piece must exist at the given square
    pub(crate) fn remove_piece(&mut self, square: Square) {
        let piece = self.piece_at(square);
        let p = piece.index();
        let c = piece.color() as usize;

        self.piece_bb[p] = self.piece_bb[p].pop_bit(square);
        self.side_bb[c] = self.side_bb[c].pop_bit(square);
        self.piece[square as usize] = None;
        self.hash.toggle_piece(piece, square);
    }

    /// Looks for which piece is on the given Square
    /// Panics if no piece is on that square
    pub fn piece_at(&self, square: Square) -> Piece {
        self.piece[square as usize].unwrap()
    }

    /// Mask all attackers of a certain square, given the blocker bitboard.
    fn attackers(&self, square: Square, blockers: BitBoard) -> BitBoard {
        self.opp_occupancy()
            & (self.knights() & knight_attacks(square)
                | self.kings() & king_attacks(square)
                | self.pawns() & pawn_attacks(square, self.side)
                | (self.queens() | self.rooks()) & rook_attacks(square, blockers)
                | (self.queens() | self.bishops()) & bishop_attacks(square, blockers))
    }

    /// Mask all checking pieces
    pub(crate) fn checkers(&self) -> BitBoard {
        self.attackers(self.own_king().lsb(), self.occupancy())
    }

    /// Returns true if the square is attacked by at least one enemy piece
    pub(crate) fn square_attacked(&self, square: Square, blockers: BitBoard) -> bool {
        self.attackers(square, blockers) != BitBoard::EMPTY
    }

    /// Finds legal move in board from the uci-formatted move string
    pub fn find_move(&self, move_str: &str) -> Option<Move> {
        self.gen_moves::<QUIETS>()
            .moves
            .into_iter()
            .find(|m| m.to_string() == move_str)
    }
}

/// SEE
impl Board {
    /// Returns bitboard with all pieces attacking a square
    fn map_all_attackers(&self, square: Square, blockers: BitBoard) -> BitBoard {
        self.pawns() & self.white() & pawn_attacks(square, Color::Black)
            | self.pawns() & self.black() & pawn_attacks(square, Color::White)
            | self.knights() & knight_attacks(square)
            | (self.bishops() | self.queens()) & bishop_attacks(square, blockers)
            | (self.rooks() | self.queens()) & rook_attacks(square, blockers)
            | self.kings() & king_attacks(square)
    }

    /// Returns the least valuable of the attackers within the attacker map
    fn get_lva(&self, attackers: BitBoard, side: Color) -> Option<(Square, Piece)> {
        let side_bb = self.side_bb[side as usize];

        for piece in Piece::SPLIT_COLOR[side as usize] {
            let squares = attackers & self.piece_bb[piece.index()] & side_bb;

            if squares != BitBoard::EMPTY {
                return Some((squares.lsb(), piece));
            }
        }

        None
    }

    /// Checks if the static exchange after a move is enough to beat the given threshold
    /// This can be used for both captures and quiet moves.
    /// This implementation is basically that seen in Viri, which in turn is that of Ethereal
    pub fn see(&self, m: Move, threshold: Eval) -> bool {
        let src = m.get_src();
        let tgt = m.get_tgt();
        let mt = m.get_type();

        // Castling cannot have bad SEE, since all squares the king passes through are not attacked
        if mt == MoveType::Castle {
            return true;
        }

        // Piece being swapped off is the promoted piece
        let victim = if mt.is_promotion() {
            mt.get_promotion(self.side)
        } else {
            self.piece_at(src)
        };

        // Get the static move value (also works for quiets)
        let mut move_value = if mt.is_capture() {
            if mt == MoveType::EnPassant {
                PIECE_VALUES[Piece::WP as usize]
            } else {
                PIECE_VALUES[self.piece_at(tgt) as usize]
            }
        } else {
            0
        };
        if mt.is_promotion() {
            move_value += PIECE_VALUES[victim as usize] - PIECE_VALUES[0];
        }

        // Lose if the balance is already in our opponent's favor and it's their turn
        let mut balance = move_value - threshold;
        if balance < 0 {
            return false;
        }

        // Win if the balance is still in our favor even if we lose the capturing piece
        // This ensures a positive SEE in case of even trades.
        balance -= PIECE_VALUES[victim as usize];
        if balance >= 0 {
            return true;
        }

        let diagonal_sliders = self.bishops() | self.queens();
        let orthogonal_sliders = self.rooks() | self.queens();

        // Updated occupancy map after capture
        let mut occs = self.occupancy().pop_bit(src).set_bit(tgt);
        if mt == MoveType::EnPassant {
            let ep_tgt = self.en_passant.unwrap().forward(!self.side); // guaranteed to be Some
            occs = occs.pop_bit(ep_tgt);
        }

        // Get all pieces covering the exchange square and start exchanging
        let mut attackers = self.map_all_attackers(tgt, occs) & occs;
        let mut side_to_move = !self.side;

        loop {
            // SEE terminates when no recapture is possible.
            let own_attackers = attackers & self.side_bb[side_to_move as usize];
            if own_attackers == BitBoard::EMPTY {
                break;
            }

            // Get the least valuable attacker and simulate the recapture
            let (attacker_square, attacker) = self.get_lva(own_attackers, side_to_move).unwrap(); // attackers are at least one
            occs = occs.pop_bit(attacker_square);

            // Diagonal recaptures uncover bishops/queens
            if attacker.is_pawn() || attacker.is_bishop() || attacker.is_queen() {
                attackers |= bishop_attacks(tgt, occs) & diagonal_sliders;
            }

            // Orthogonal recaptures uncover rooks/queens
            if attacker.is_rook() || attacker.is_queen() {
                attackers |= rook_attacks(tgt, occs) & orthogonal_sliders;
            }
            attackers &= occs; // ignore pieces already "used up"

            // Negamax the balance, cutoff if losing our attacker would still win the exchange
            side_to_move = !side_to_move;
            balance = -balance - 1 - PIECE_VALUES[attacker as usize];
            if balance >= 0 {
                // If the recapturing piece is a king, and the opponent has another attacker,
                // a positive balance should not translate to an exchange win.
                if attacker.is_king()
                    && attackers & self.side_bb[side_to_move as usize] != BitBoard::EMPTY
                {
                    return self.side == side_to_move;
                }

                break;
            }
        }

        // We win the exchange if we are not the one who should recapture
        self.side != side_to_move
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invalid_fen() {
        let invalid_pieces =
            "rnbqkbnr/pp2ppppp/7/2p5/4P3/5N2/PPPP1P/RNBQKB1R b - - 1 2".parse::<Board>();
        let invalid_side =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1".parse::<Board>();
        let invalid_castle =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w kqKQ - 0 1".parse::<Board>();
        let invalid_ep_square =
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQ a9 0 1".parse::<Board>();

        assert!(invalid_pieces.is_err());
        assert!(invalid_side.is_err());
        assert!(invalid_castle.is_err());
        assert!(invalid_ep_square.is_err());
    }

    #[test]
    fn test_see_helpers() {
        let b1: Board = "1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1"
            .parse()
            .unwrap();
        let b2: Board = "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1"
            .parse()
            .unwrap();

        let att1 = b1.map_all_attackers(Square::E5, b1.occupancy());
        let att2 = b2.map_all_attackers(Square::E5, b2.occupancy());

        println!("{b1}\n{att1}\n{b2}\n{att2}");

        assert!(att1.get_bit(Square::E1));
        assert!(!att1.get_bit(Square::D8));

        assert!(att2.get_bit(Square::E2));
        assert!(!att2.get_bit(Square::E1));
    }

    #[test]
    fn test_see() {
        #[rustfmt::skip]
        const SEE_SUITE: [(&str, &str, Eval, bool); 13] = [
            ("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1", "e1e5", 0, true),
            ("1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1", "d3e5", 0, false),
            ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", "g2h3", 0, true),
            ("k3r3/8/8/4p3/8/2B5/1B6/K7 w - - 0 1", "c3e5", 0, true),
            ("4kbnr/p1P4p/b1q5/5pP1/4n3/5Q2/PP1PPP1P/RNB1KBNR w KQk f6 0 1", "g5f6", 0, true),
            ("6k1/1pp4p/p1pb4/6q1/3P1pRr/2P4P/PP1Br1P1/5RKN w - - 0 1", "f1f4", 0, false),
            ("6RR/4bP2/8/8/5r2/3K4/5p2/4k3 w - - 0 1", "f7f8q", 0, true),
            ("r1bqk1nr/pppp1ppp/2n5/1B2p3/1b2P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1", "e1g1", 0, true),
            ("4kbnr/p1P1pppp/b7/4q3/7n/8/PPQPPPPP/RNB1KBNR w KQk - 0 1", "c7c8q", 0, true),
            ("4kbnr/p1P1pppp/b7/4q3/7n/8/PP1PPPPP/RNBQKBNR w KQk - 0 1", "c7c8q", 0, false),
            ("3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - - 0 1", "d3d4", 0, false),
            ("5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - - 0 1", "d6f4", 0, false),
            ("5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - - 0 1", "d6f4", -108, true),
        ];

        for (b, m, t, r) in SEE_SUITE {
            let board: Board = b.parse().unwrap();
            let m = board.find_move(m).unwrap();

            println!("Move: {m}{board}");
            assert_eq!(board.see(m, t), r);
        }
    }
}
