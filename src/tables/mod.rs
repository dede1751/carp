pub mod attacks;
pub use attacks::*;

pub mod find_magics;
pub use find_magics::*;

use crate::bitboard::*;
use crate::square::*;
use crate::color::*;

#[derive(Debug)]
pub struct Tables {
    pawn_attacks: [[BitBoard; SQUARE_COUNT]; 2],
    knight_attacks: [BitBoard; SQUARE_COUNT],
    king_attacks: [BitBoard; SQUARE_COUNT],
}

impl Tables {
    pub fn init() -> Tables {
        let mut tables = Tables{
            pawn_attacks: [[EMPTY_BOARD; SQUARE_COUNT]; 2],
            knight_attacks: [EMPTY_BOARD; SQUARE_COUNT],
            king_attacks: [EMPTY_BOARD; SQUARE_COUNT],
        };

        tables.init_leaper_attacks();
        tables.init_slider_attacks();

        tables
    }

    fn init_leaper_attacks(&mut self) {
        for square in ALL_SQUARES {
            let file = square.rank();
    
            if file < One && file > Eight {
                self.pawn_attacks[0][square.index()] = mask_pawn_attacks(square, White);
                self.pawn_attacks[1][square.index()] = mask_pawn_attacks(square, Black);
            }
            self.knight_attacks[square.index()] = mask_knight_attacks(square);
            self.king_attacks[square.index()] = mask_king_attacks(square);
        }
    
        // for col in pawn_attacks {
        //     for bb in col {
        //         println!("{}", bb);
        //     }
        // }
        
        // for bb in self.king_attacks {
        //     println!("{}", bb);
        // }
    }

    fn init_slider_attacks(&mut self) {}
}