/// Implement both static attack lookups and board move generation.
use std::mem::transmute;

use super::magics::Magics;

use crate::{
    bitboard::{BitBoard, BB64},
    board::Board,
    move_list::MoveList,
    moves::{Move, MoveType},
    piece::{Color, Piece},
    square::Square,
};

/// Bitboard of squares between two squares, excluding the squares themselves
const BETWEEN: [BB64; Square::COUNT] =
    unsafe { transmute(*include_bytes!("../../../bins/between.bin")) };

/// Attacks for the hopping pieces are just precalculated bitboards.
const KING_ATTACKS: BB64 = unsafe { transmute(*include_bytes!("../../../bins/king.bin")) };
const KNIGHT_ATTACKS: BB64 = unsafe { transmute(*include_bytes!("../../../bins/knight.bin")) };
const PAWN_ATTACKS: [BB64; 2] = unsafe { transmute(*include_bytes!("../../../bins/pawn.bin")) };

/// Gets pawn attacks from tables
/// SAFETY: Square and Color only allow valid indices
pub(crate) fn pawn_attacks(square: Square, side: Color) -> BitBoard {
    unsafe {
        *PAWN_ATTACKS
            .get_unchecked(side as usize)
            .get_unchecked(square as usize)
    }
}

/// Gets knight attacks from tables
/// SAFETY: Square only allows valid indices
pub(crate) fn knight_attacks(square: Square) -> BitBoard {
    unsafe { *KNIGHT_ATTACKS.get_unchecked(square as usize) }
}

/// Gets king attacks from tables
/// SAFETY: Square only allows valid indices
pub(crate) fn king_attacks(square: Square) -> BitBoard {
    unsafe { *KING_ATTACKS.get_unchecked(square as usize) }
}

/// Gets bishop attacks based on the blocker bitboard
pub(crate) fn bishop_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    Magics::BISHOP.attacks(square, blockers)
}

/// Gets rook attacks based on the blocker bitboard
pub(crate) fn rook_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    Magics::ROOK.attacks(square, blockers)
}

pub const QUIETS: bool = true;
pub const CAPTURES: bool = false;
const KS: usize = 0;
const QS: usize = 1;

impl Board {
    /// Generate all legal king moves, or only captures if QUIET==false
    fn gen_king_moves<const QUIET: bool>(&self, move_list: &mut MoveList) {
        let src = self.own_king().lsb();
        let attacks = king_attacks(src);
        let blockers = self.occupancy().pop_bit(src);

        for tgt in attacks & self.opp_occupancy() {
            if !self.square_attacked(tgt, blockers) {
                move_list.push(Move::new(src, tgt, MoveType::Capture));
            }
        }

        if QUIET {
            for tgt in attacks & !self.occupancy() {
                if !self.square_attacked(tgt, blockers) {
                    move_list.push(Move::new(src, tgt, MoveType::Quiet));
                }
            }
        }
    }

    /// Generate castling moves for the given castling side.
    fn gen_castling_moves<const KS: usize>(&self, move_list: &mut MoveList) {
        const SRC: [Square; 2] = [Square::E1, Square::E8];
        const MID: [[Square; 2]; 2] = [[Square::F1, Square::F8], [Square::D1, Square::D8]];
        const TGT: [[Square; 2]; 2] = [[Square::G1, Square::G8], [Square::C1, Square::C8]];
        const OCCS: [[BitBoard; 2]; 2] = [
            [BitBoard(0x6000000000000000), BitBoard(0x0000000000000060)],
            [BitBoard(0x0E00000000000000), BitBoard(0x000000000000000E)],
        ];
        let side = self.side as usize;

        if self.occupancy() & OCCS[KS][side] == BitBoard::EMPTY
            && !self.square_attacked(MID[KS][side], self.occupancy())
            && !self.square_attacked(TGT[KS][side], self.occupancy())
        {
            move_list.push(Move::new(SRC[side], TGT[KS][side], MoveType::Castle));
        }
    }

    /// Generate all legal pawn advancing moves, including double pushes and promotions.
    fn gen_pawn_advances(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        move_list: &mut MoveList,
    ) {
        let pawns = self.own_pawns() & !diag_pins;
        let empty = !self.occupancy();
        let possible_targets = empty & check_mask;
        let single_pushes = pawns & possible_targets.forward(!self.side);

        // Handle promotions
        for src in single_pushes & BitBoard::PROMO_RANKS[self.side as usize] {
            let tgt = src.forward(self.side);
            if !hv_pins.get_bit(src) || hv_pins.get_bit(tgt) {
                move_list.push(Move::new(src, tgt, MoveType::QueenPromotion));
                move_list.push(Move::new(src, tgt, MoveType::KnightPromotion));
                move_list.push(Move::new(src, tgt, MoveType::RookPromotion));
                move_list.push(Move::new(src, tgt, MoveType::BishopPromotion));
            }
        }

        // Handle single pushes
        for src in single_pushes & !BitBoard::PROMO_RANKS[self.side as usize] {
            let tgt = src.forward(self.side);
            if !hv_pins.get_bit(src) || hv_pins.get_bit(tgt) {
                move_list.push(Move::new(src, tgt, MoveType::Quiet));
            }
        }

        // Handle double pushes
        let double_pushes = pawns
            & (empty & possible_targets.forward(!self.side)).forward(!self.side)
            & BitBoard::START_RANKS[self.side as usize];

        for src in double_pushes {
            let tgt = src.forward(self.side).forward(self.side);
            if !hv_pins.get_bit(src) || hv_pins.get_bit(tgt) {
                move_list.push(Move::new(src, tgt, MoveType::DoublePush));
            }
        }
    }

    /// Generate all pawn diagonal captures, including enpassant.
    fn gen_pawn_captures(
        &self,
        check_mask: BitBoard,
        diag_pins: BitBoard,
        hv_pins: BitBoard,
        move_list: &mut MoveList,
    ) {
        let pawns = self.own_pawns() & !hv_pins;
        let possible_targets = self.opp_occupancy() & check_mask;

        // Capture promotions
        for src in pawns & BitBoard::PROMO_RANKS[self.side as usize] {
            let mut attacks = pawn_attacks(src, self.side);

            if diag_pins.get_bit(src) {
                attacks &= diag_pins
            }

            for tgt in attacks & possible_targets {
                move_list.push(Move::new(src, tgt, MoveType::QueenCapPromo));
                move_list.push(Move::new(src, tgt, MoveType::KnightCapPromo));
                move_list.push(Move::new(src, tgt, MoveType::RookCapPromo));
                move_list.push(Move::new(src, tgt, MoveType::BishopCapPromo));
            }
        }

        // Enpassant
        if let Some(tgt) = self.en_passant {
            let ep_tgt = tgt.forward(!self.side);

            if check_mask & (tgt.to_board() | ep_tgt.to_board()) != BitBoard::EMPTY {
                for src in pawns & pawn_attacks(tgt, !self.side) {
                    // Restrict enpassant to the pin ray
                    if diag_pins.get_bit(src) && !diag_pins.get_bit(tgt) {
                        continue;
                    }

                    // En Passant discovered check!
                    let ep_rank = BitBoard::EP_RANKS[self.side as usize];
                    if self.own_king() & ep_rank != BitBoard::EMPTY
                        && self.opp_queen_rook() & ep_rank != BitBoard::EMPTY
                    {
                        // Remove the two pawns and see if the king sees an enemy orthogonal slider.
                        let blockers = self.occupancy() & !src.to_board() & !ep_tgt.to_board();
                        let king_square = self.own_king().lsb();
                        let king_ray = ep_rank & rook_attacks(king_square, blockers);

                        if king_ray & self.opp_queen_rook() != BitBoard::EMPTY {
                            continue;
                        }
                    }
                    move_list.push(Move::new(src, tgt, MoveType::EnPassant));
                }
            }
        }

        // Normal captures
        for src in pawns & !BitBoard::PROMO_RANKS[self.side as usize] {
            let mut attacks = pawn_attacks(src, self.side);

            if diag_pins.get_bit(src) {
                attacks &= diag_pins
            }

            for tgt in attacks & possible_targets {
                move_list.push(Move::new(src, tgt, MoveType::Capture));
            }
        }
    }

    /// Generate all legal moves for any standard piece.
    /// PIECE is either Piece::N, Piece::B, Piece::R. Rooks and Bishops also gen queen moves.
    fn gen_piece<const PIECE: usize, const QUIET: bool>(
        &self,
        check_mask: BitBoard,
        move_pin_mask: BitBoard,
        stuck_pin_mask: BitBoard,
        move_list: &mut MoveList,
    ) {
        let pieces = match PIECE {
            Piece::N => self.own_knights(),
            Piece::B => self.own_queens() | self.own_bishops(),
            Piece::R => self.own_queens() | self.own_rooks(),
            _ => unreachable!(),
        } & !stuck_pin_mask;
        let blockers = self.occupancy();

        for src in pieces {
            let mut attacks = match PIECE {
                Piece::N => knight_attacks(src),
                Piece::B => bishop_attacks(src, blockers),
                Piece::R => rook_attacks(src, blockers),
                _ => unreachable!(),
            } & check_mask;

            if move_pin_mask.get_bit(src) {
                attacks &= move_pin_mask;
            }

            for tgt in attacks & self.opp_occupancy() {
                move_list.push(Move::new(src, tgt, MoveType::Capture));
            }

            if QUIET {
                for tgt in attacks & !self.occupancy() {
                    move_list.push(Move::new(src, tgt, MoveType::Quiet));
                }
            }
        }
    }

    /// Generates pinned pieces and diagonal/orthogonal pin maps
    ///
    /// Pin masks are defined as the squares between a pinning enemy piece and one's own king.
    /// Any pinned piece can safely move along these squares (simply & moves with pinmask).
    /// For simplicity, pin masks also indirectly include the check mask (this has no actual
    /// effect on the pin use, as no piece can be sitting on the check mask anyways)
    fn map_pins(&self) -> (BitBoard, BitBoard) {
        let king_square = self.own_king().lsb();
        let occs = self.occupancy();

        // get all own pieces on diagonal/orthogonal rays from the king
        let possible_diag_pins = bishop_attacks(king_square, occs) & self.own_occupancy();
        let possible_hv_pins = rook_attacks(king_square, occs) & self.own_occupancy();

        // remove the possible pinned pieces
        let remove_diag_blockers = occs & !possible_diag_pins;
        let remove_hv_blockers = occs & !possible_hv_pins;

        // get all pinning pieces (pieces that see the king with pinned pieces removed)
        let diag_attackers =
            bishop_attacks(king_square, remove_diag_blockers) & self.opp_queen_bishop();
        let hv_attackers = rook_attacks(king_square, remove_hv_blockers) & self.opp_queen_rook();

        // pin masks are between the attacker and the king square (attacker included)
        let diag_pins = diag_attackers
            .into_iter()
            .map(|sq| BETWEEN[king_square as usize][sq as usize])
            .fold(BitBoard::EMPTY, |acc, x| acc | x);

        let hv_pins = hv_attackers
            .into_iter()
            .map(|sq| BETWEEN[king_square as usize][sq as usize])
            .fold(BitBoard::EMPTY, |acc, x| acc | x);

        (diag_pins, hv_pins)
    }

    /// Inner move generation, generic over the side to move.
    pub fn gen_moves<const QUIET: bool>(&self) -> MoveList {
        let mut ml = MoveList::default();
        let move_list = &mut ml;

        self.gen_king_moves::<QUIET>(move_list);

        // With double checks, only king moves are legal, so we stop here
        let attacker_count = self.checkers.count_bits();
        if attacker_count > 1 {
            return ml;
        }

        // Generate all the legal piece moves using pin and blocker/capture masks
        let check_mask = if attacker_count == 1 {
            BETWEEN[self.own_king().lsb() as usize][self.checkers.lsb() as usize] | self.checkers
        } else {
            BitBoard::FULL
        };
        let (diag_pins, hv_pins) = self.map_pins();
        let all_pins = diag_pins | hv_pins;

        if QUIET && attacker_count == 0 {
            if self.castling_rights.has_kingside(self.side) {
                self.gen_castling_moves::<KS>(move_list);
            }
            if self.castling_rights.has_queenside(self.side) {
                self.gen_castling_moves::<QS>(move_list);
            }
        }

        self.gen_pawn_captures(check_mask, diag_pins, hv_pins, move_list);
        if QUIET {
            self.gen_pawn_advances(check_mask, diag_pins, hv_pins, move_list);
        }

        self.gen_piece::<{ Piece::N }, QUIET>(check_mask, BitBoard::EMPTY, all_pins, move_list);
        self.gen_piece::<{ Piece::B }, QUIET>(check_mask, diag_pins, hv_pins, move_list);
        self.gen_piece::<{ Piece::R }, QUIET>(check_mask, hv_pins, diag_pins, move_list);

        ml
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pin_mask() {
        let board: Board = "R2bk3/5p2/4r1B1/1Q6/8/4Q3/4R3/2K5 b - - 0 1"
            .parse()
            .unwrap();
        println!("{board}");

        let (diag_pins, hv_pins) = board.map_pins();
        let pinned = (diag_pins | hv_pins) & board.own_occupancy();
        println!("{}\n{}\n{}", pinned, diag_pins, hv_pins);

        assert!(pinned.get_bit(Square::F7));
        assert!(pinned.get_bit(Square::E6));
        assert!(diag_pins.get_bit(Square::G6));
        assert!(hv_pins.get_bit(Square::C8));
        assert!(hv_pins.get_bit(Square::E3));
        assert!(!hv_pins.get_bit(Square::E2));
    }

    #[test]
    fn test_legal_pawn() {
        let b1: Board = "8/8/8/1k6/3Pp3/8/8/4KQ2 b - d3 0 1".parse().unwrap();
        println!("{b1}");
        let m1 = b1.gen_moves::<QUIETS>(); // enpassant blocks check
        assert_eq!(m1.len(), 6);

        let b2: Board = "8/8/8/2k5/3Pp3/8/8/4K3 b - d3 0 1".parse().unwrap();
        println!("{b2}");
        let m2 = b2.gen_moves::<QUIETS>(); // enpassant captures checker
        assert_eq!(m2.len(), 9);

        let b3: Board = "8/8/8/8/k2Pp2Q/8/8/3K4 b - d3 0 1".parse().unwrap();
        println!("{b3}");
        let m3 = b3.gen_moves::<QUIETS>(); // enpassant would leave the king in check
        assert_eq!(m3.len(), 6);
    }
}
