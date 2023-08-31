#![allow(
    dead_code,
    unused_imports,
    clippy::unused_self,
    unused_variables,
    clippy::missing_const_for_fn
)]
/// Wrapper over the Fathom interface.
/// Most of the code is an adaptation of Wahoo/Viridithas, which in turn are based on the Stockfish
/// implementation.
use std::{error::Error, ffi::CString, ptr, sync::atomic::AtomicU64};

use super::bindings::{
    tb_init, tb_probe_root, tb_probe_wdl, TB_BLESSED_LOSS, TB_CURSED_WIN, TB_DRAW, TB_LARGEST,
    TB_LOSS, TB_PROMOTES_BISHOP, TB_PROMOTES_KNIGHT, TB_PROMOTES_QUEEN, TB_PROMOTES_ROOK,
    TB_RESULT_DTZ_MASK, TB_RESULT_DTZ_SHIFT, TB_RESULT_FAILED, TB_RESULT_FROM_MASK,
    TB_RESULT_FROM_SHIFT, TB_RESULT_PROMOTES_MASK, TB_RESULT_PROMOTES_SHIFT, TB_RESULT_TO_MASK,
    TB_RESULT_TO_SHIFT, TB_RESULT_WDL_MASK, TB_RESULT_WDL_SHIFT, TB_WIN,
};

use crate::search_params::{Eval, MATE, TB_MATE};
use chess::{
    board::{Board, QUIETS},
    castle::CastlingRights,
    moves::Move,
    piece::{Color, Piece},
    square::Square,
};

/// Used to keep track of the number of TB hits across threads.
pub static TB_HITS: AtomicU64 = AtomicU64::new(0);

/// Possible TB outcomes, witout specifying BLESSED_LOSS/CURSED_WIN.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub enum WDL {
    Win,
    Loss,
    Draw,
}

impl WDL {
    /// Convert from a simple WDL (resulting from tb_probe_wdl) to a WDL.
    fn new(result: u32) -> Option<Self> {
        match result {
            TB_WIN => Some(WDL::Win),
            TB_LOSS => Some(WDL::Loss),
            TB_DRAW | TB_CURSED_WIN | TB_BLESSED_LOSS => Some(WDL::Draw),
            _ => None,
        }
    }

    /// Convert a syzygy WDL result to a search evaluation, factoring in the root distance.
    pub fn to_eval(self, ply: usize) -> Eval {
        match self {
            WDL::Win => TB_MATE - ply as Eval,
            WDL::Loss => -TB_MATE + ply as Eval,
            WDL::Draw => 0,
        }
    }
}

/// Full root probe result, consisting of both WDL and DTZ.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Debug, Hash)]
pub struct TBProbe {
    pub wdl: WDL,
    pub dtz: u32,
    pub best_move: Move,
}

impl TBProbe {
    /// Convert from a full probe result (resulting from tb_probe_root) to a TBProbe.
    fn new(result: u32, board: &Board) -> Option<Self> {
        if result == TB_RESULT_FAILED {
            return None;
        }

        let wdl = match (result & TB_RESULT_WDL_MASK) >> TB_RESULT_WDL_SHIFT {
            TB_WIN => WDL::Win,
            TB_LOSS => WDL::Loss,
            _ => WDL::Draw,
        };
        let dtz = (result & TB_RESULT_DTZ_MASK) >> TB_RESULT_DTZ_SHIFT;

        let from = Square::from(((result & TB_RESULT_FROM_MASK) >> TB_RESULT_FROM_SHIFT) as usize);
        let to = Square::from(((result & TB_RESULT_TO_MASK) >> TB_RESULT_TO_SHIFT) as usize);
        let mut move_str = from.to_string() + to.to_string().as_str();

        let promotion = (result & TB_RESULT_PROMOTES_MASK) >> TB_RESULT_PROMOTES_SHIFT;
        match promotion {
            TB_PROMOTES_QUEEN => move_str.push(board.side.queen().to_char()),
            TB_PROMOTES_ROOK => move_str.push(board.side.rook().to_char()),
            TB_PROMOTES_BISHOP => move_str.push(board.side.bishop().to_char()),
            TB_PROMOTES_KNIGHT => move_str.push(board.side.knight().to_char()),
            _ => {}
        };

        board.find_move(move_str.as_str()).map(|mv| Self {
            wdl,
            dtz,
            best_move: mv,
        })
    }

    /// Convert a syzygy WDL-DTZ result to a search evaluation.
    pub fn to_eval(self) -> Eval {
        match self.wdl {
            WDL::Win => MATE - self.dtz as Eval,
            WDL::Loss => -MATE + self.dtz as Eval,
            WDL::Draw => 0,
        }
    }
}

/// Main Syzygy tablebases interface.
/// Thin Copy wrapper around the Fathom interface.
#[derive(Copy, Clone, Default, Debug)]
pub struct TB {
    active: bool,
    n_men: u8,
}

impl TB {
    pub const MAX_MEN: u8 = 6;

    /// Initialize the tablebases stored at the given path.
    /// Limit the tablebases to whatever piece count is specified.
    #[cfg(feature = "syzygy")]
    pub fn activate(&mut self, path: &str, syzygy_probe_limit: u8) {
        unsafe {
            let syzygy_path = CString::new(path).unwrap();
            assert!(
                tb_init(syzygy_path.as_ptr()),
                "Cannot initialize TBs at: {path}"
            );
        }
        self.active = true;
        self.n_men = unsafe { TB_LARGEST as u8 }.min(syzygy_probe_limit);
    }

    /// Check if the tablebases can be used to probe the given position.
    #[cfg(feature = "syzygy")]
    fn can_probe(self, board: &Board) -> bool {
        self.active
            && self.n_men >= (board.occupancy().count_bits() as u8)
            && board.castling_rights == CastlingRights::NONE
    }

    /// Probe the tablebases for the WDL result (used within the search tree)
    #[cfg(feature = "syzygy")]
    pub fn probe_wdl(self, board: &Board) -> Option<WDL> {
        if !self.can_probe(board) || board.halfmoves != 0 {
            return None;
        }

        let ep_sq = if let Some(sq) = board.en_passant {
            sq as u32
        } else {
            0
        };

        unsafe {
            let wdl = tb_probe_wdl(
                board.white().0,
                board.black().0,
                board.kings().0,
                board.queens().0,
                board.rooks().0,
                board.bishops().0,
                board.knights().0,
                board.pawns().0,
                0,
                0,
                ep_sq,
                board.side == Color::White,
            );

            WDL::new(wdl)
        }
    }

    /// Probe the tablebases for the full WDL DTZ result with the best move.
    #[cfg(feature = "syzygy")]
    pub fn probe_root(self, board: &Board) -> Option<TBProbe> {
        if !self.can_probe(board) {
            return None;
        }

        let ep_sq = if let Some(sq) = board.en_passant {
            sq as u32
        } else {
            0
        };

        unsafe {
            let result = tb_probe_root(
                board.white().0,
                board.black().0,
                board.kings().0,
                board.queens().0,
                board.rooks().0,
                board.bishops().0,
                board.knights().0,
                board.pawns().0,
                board.halfmoves as u32,
                0,
                ep_sq,
                board.side == Color::White,
                ptr::null_mut(),
            );

            TBProbe::new(result, board)
        }
    }

    #[cfg(not(feature = "syzygy"))]
    pub fn activate(&mut self, path: &str, syzygy_probe_limit: u8) {}

    #[cfg(not(feature = "syzygy"))]
    const fn can_probe(self, board: &Board) -> bool {
        false
    }

    #[cfg(not(feature = "syzygy"))]
    pub fn probe_wdl(self, board: &Board) -> Option<WDL> {
        None
    }

    #[cfg(not(feature = "syzygy"))]
    pub fn probe_root(self, board: &Board) -> Option<TBProbe> {
        None
    }
}

#[cfg(test)]
#[cfg(feature = "syzygy")]
mod tests {
    use super::*;

    #[test]
    fn can_probe() {
        let path = "/home/dede/Documents/Syzygy";
        let mut tb = TB::default();
        tb.activate(path, TB::MAX_MEN);

        let board: Board = "8/3kr3/8/8/8/8/2K5/2Q5 w - - 0 1".parse().unwrap();
        println!("{board}");

        if let Some(result) = tb.probe_root(&board) {
            println!("{}", result.best_move);
            assert_eq!(result.wdl, WDL::Win);
            assert_eq!(result.wdl, tb.probe_wdl(&board).unwrap());
        } else {
            panic!("Probe failed")
        }
    }

    #[test]
    fn halfmove_handling() {
        let path = "/home/dede/Documents/Syzygy";
        let mut tb = TB::default();
        tb.activate(path, TB::MAX_MEN);

        let mut board: Board = "8/3kr3/8/8/8/8/2K5/2Q5 w - - 0 1".parse().unwrap();
        board.halfmoves = 70;
        println!("{board}");

        if let Some(result) = tb.probe_root(&board) {
            println!("{}", result.best_move);
            assert_eq!(result.wdl, WDL::Draw);
            assert!(tb.probe_wdl(&board).is_none());
        } else {
            panic!("Probe failed")
        }
    }
}
