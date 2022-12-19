/// Slow attack generation to initialize attack tables

use crate::{
    bitboard::{ BitBoard, EMPTY_BB },
    square::*,
    piece::Color,
};

/// Generate bitboard for pawn attacks from square
pub fn mask_pawn_attacks(square: Square, color: Color) -> BitBoard {
    let bitboard = square.to_board();
    let mut attacks = EMPTY_BB;
    let file = square.file();

    match color {
        Color::White => {
            if file != File::A { attacks |= bitboard >> BitBoard(9) }
            if file != File::H { attacks |= bitboard >> BitBoard(7) }
        },
        Color::Black => {
            if file != File::H { attacks |= bitboard << BitBoard(9) }
            if file != File::A { attacks |= bitboard << BitBoard(7) }
        }
    }

    attacks
}

/// Generate bitboard for knight attacks from square
pub fn mask_knight_attacks(square: Square) -> BitBoard {
    ALL_SQUARES.iter()
               .filter(|&tgt| { 
        let dist: (i8, i8) = square.dist(*tgt);

        (dist.0.abs() == 1 && dist.1.abs() == 2) | (dist.0.abs() == 2 && dist.1.abs() == 1)
    })
               .fold(EMPTY_BB, |mask, square| { mask ^ square.to_board() })
}

/// Generate bitboard for king attacks from square
pub fn mask_king_attacks(square: Square) -> BitBoard {
    ALL_SQUARES.iter()
               .filter(|&tgt| { 
        let dist: (i8, i8) = square.dist(*tgt);

        (dist.0.abs() == 1 && ((dist.1.abs() == 0) | (dist.1.abs() == 1))) |
        (dist.1.abs() == 1 && ((dist.0.abs() == 0) | (dist.0.abs() == 1)))
    })
               .fold(EMPTY_BB, |mask, square| { mask ^ square.to_board() })
}


/// Generate bishop attacks on the fly
pub fn mask_bishop_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    let mut attacks = EMPTY_BB;
    let (file, rank) = (square.file(), square.rank());
    
    // top right diagonal
    let (mut f, mut r) = (file, rank);
    while f != File::H  && r != Rank::Eight {
        (f, r) = (f.right(), r.up());
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BB { break; }
    }

    // bottom left diagonal
    (f, r) = (file, rank);
    while f != File::A && r != Rank::First {
        (f, r) = (f.left(), r.down());
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BB { break; }
    }

    // top left diagonal
    (f, r) = (file, rank);
    while f != File::A && r != Rank::Eight {
        (f, r) = (f.left(), r.up());
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BB { break; }
    }
    
    // bottom right diagonal
    (f, r) = (file, rank);
    while f != File::H && r != Rank::First {
        (f, r) = (f.right(), r.down());
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BB { break; }
    }
    
    attacks
}

/// Generate rook attacks on the fly
pub fn mask_rook_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    let mut attacks = EMPTY_BB;
    let (file, rank) = (square.file(), square.rank());
    let (mut f, mut r) = (file, rank);    
    
    // right
    while f != File::H {
        f = f.right();
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BB { break; }
    }
    
    // left
    f = file;
    while f != File::A {
        f = f.left();
        let s = Square::from_coords(f, r);

        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BB { break; }
    }
    
    // up
    f = file;
    while r != Rank::Eight {
        r = r.up();
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BB { break; }
    }
    
    // down
    r = rank;
    while r != Rank::First {
        r = r.down();
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BB { break; }
    }
    
    attacks
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pawn_attacks() {
        let bb1: BitBoard = mask_pawn_attacks(Square::A2, Color::White);
        let bb2: BitBoard = mask_pawn_attacks(Square::E4, Color::White);
        let bb3: BitBoard = mask_pawn_attacks(Square::H7, Color::Black);
        let bb4: BitBoard = mask_pawn_attacks(Square::E7, Color::White);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(2199023255552));
        assert_eq!(bb2, BitBoard(671088640));
        assert_eq!(bb3, BitBoard(4194304));
        assert_eq!(bb4, BitBoard(40));
    }

    #[test]
    fn knight_attacks() {
        let bb1: BitBoard = mask_knight_attacks(Square::A1);
        let bb2: BitBoard = mask_knight_attacks(Square::E4);
        let bb3: BitBoard = mask_knight_attacks(Square::G6);
        let bb4: BitBoard = mask_knight_attacks(Square::B7);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(1128098930098176));
        assert_eq!(bb2, BitBoard(11333767002587136));
        assert_eq!(bb3, BitBoard(687463207072));
        assert_eq!(bb4, BitBoard(84410376));
    }

    #[test]
    fn king_attacks(){
        let bb1: BitBoard = mask_king_attacks(Square::A1);
        let bb2: BitBoard = mask_king_attacks(Square::E4);
        let bb3: BitBoard = mask_king_attacks(Square::H6);
        let bb4: BitBoard = mask_king_attacks(Square::D8);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(144959613005987840));
        assert_eq!(bb2, BitBoard(61745389371392));
        assert_eq!(bb3, BitBoard(3225468928));
        assert_eq!(bb4, BitBoard(7188));
    }

    #[test]
    fn bishop_attacks(){
        let bb1: BitBoard = mask_bishop_attacks(Square::E4, BitBoard(1161084283129857));
        let bb2: BitBoard = mask_bishop_attacks(Square::B7, BitBoard(35253091631104));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1169881047499265));
        assert_eq!(bb2, BitBoard(68854022149));
    }

    #[test]
    fn rook_attacks(){
        let bb1: BitBoard = mask_rook_attacks(Square::A8, BitBoard(1099511627778));
        let bb2: BitBoard = mask_rook_attacks(Square::E4, BitBoard(76561335399223296));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1103823438082));
        assert_eq!(bb2, BitBoard(4521393946365952));
    }
}