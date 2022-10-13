use crate::bitboard::*;
use crate::square::*;
use crate::color::*;


pub fn mask_pawn_attacks(square: Square, color: Color) -> BitBoard {
    let bitboard = square.to_board();
    let mut attacks = EMPTY_BOARD;

    let file = square.file();
    match color {
        White => {
            if file != A { attacks |= bitboard >> BitBoard(9) }
            if file != H { attacks |= bitboard >> BitBoard(7) }
        },
        Black => {
            if file != H { attacks |= bitboard << BitBoard(9) }
            if file != A { attacks |= bitboard << BitBoard(7) }
        }
    }
    attacks
}

/// Generate bitboard for knight attacks from square
/// 
/// We avoid rank wrapping through bitwise operations, so out of bounds only need to be checked
/// off the sides.
pub fn mask_knight_attacks(square: Square) -> BitBoard {
    let mut bitboard = EMPTY_BOARD;
    let mut attacks = EMPTY_BOARD;

    bitboard.set_bit(square);
    let file = square.file();

    // generate "up" moves, if we're on the 7th rank bitboard >> BitBoard(17) will be 0, no wrap
    if file != A { attacks |= bitboard >> BitBoard(17); } // up up left
    if file != H { attacks |= bitboard >> BitBoard(15); } // up up right
    if file != A && file != B { attacks |= bitboard >> BitBoard(10); } // up left left
    if file != H && file != G { attacks |= bitboard >> BitBoard(6); }  // up right right

    // generate "down" moves
    if file != H { attacks |= bitboard << BitBoard(17); } // down down right
    if file != A { attacks |= bitboard << BitBoard(15); } // down down left
    if file != H && file != G { attacks |= bitboard << BitBoard(10); } // down right right
    if file != A && file != B { attacks |= bitboard << BitBoard(6); }  // down left left

    attacks
}

/// Generate bitboard for king attacks from square
/// 
/// Basically the same as knight
pub fn mask_king_attacks(square: Square) -> BitBoard {
    let mut bitboard = EMPTY_BOARD;
    let mut attacks = EMPTY_BOARD;

    bitboard.set_bit(square);
    let file = square.file();

    // up/down moves are always valid
    attacks |= bitboard >> BitBoard(8); // up
    attacks |= bitboard << BitBoard(8); // down
    
    // generate "left" moves
    if file != A { 
        attacks |= bitboard >> BitBoard(9); // up left
        attacks |= bitboard >> BitBoard(1); // left
        attacks |= bitboard << BitBoard(7); // down left
    }
    
    //generate "right" moves
    if file != H { 
        attacks |= bitboard << BitBoard(9); // down right
        attacks |= bitboard << BitBoard(1); // right
        attacks |= bitboard >> BitBoard(7); // up right
    }

    attacks
}


/// Generate bishop attacks on the fly
pub fn mask_bishop_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    let mut attacks = EMPTY_BOARD;
    let (file, rank) = (square.file(), square.rank());
    
    // top right diagonal
    let (mut f, mut r) = (file, rank);
    while f != H  && r != Eight {
        (f, r) = (f.right(), r.up());
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BOARD { break; }
    }

    // bottom left diagonal
    (f, r) = (file, rank);
    while f != A && r != One {
        (f, r) = (f.left(), r.down());
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BOARD { break; }
    }

    // top left diagonal
    (f, r) = (file, rank);
    while f != A && r != Eight {
        (f, r) = (f.left(), r.up());
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BOARD { break; }
    }
    
    // bottom right diagonal
    (f, r) = (file, rank);
    while f != H && r != One {
        (f, r) = (f.right(), r.down());
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BOARD { break; }
    }
    
    attacks
}

/// Generate rook attacks on the fly
pub fn mask_rook_attacks(square: Square, blockers: BitBoard) -> BitBoard {
    let mut attacks = EMPTY_BOARD;
    let (file, rank) = (square.file(), square.rank());
    let (mut f, mut r) = (file, rank);    
    
    // right
    while f != H {
        f = f.right();
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BOARD { break; }
    }
    
    // left
    f = file;
    while f != A {
        f = f.left();
        let s = Square::from_coords(f, r);

        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BOARD { break; }
    }
    
    // up
    f = file;
    while r != Eight {
        r = r.up();
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BOARD { break; }
    }
    
    // down
    r = rank;
    while r != One {
        r = r.down();
        let s = Square::from_coords(f, r);
        
        attacks.set_bit(s);
        if blockers & s.to_board() != EMPTY_BOARD { break; }
    }
    
    attacks
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pawn_attacks() {
        let bb1: BitBoard = mask_pawn_attacks(A2, White);
        let bb2: BitBoard = mask_pawn_attacks(E4, White);
        let bb3: BitBoard = mask_pawn_attacks(H7, Black);

        assert_eq!(bb1, BitBoard(2199023255552));
        assert_eq!(bb2, BitBoard(671088640));
        assert_eq!(bb3, BitBoard(4194304));
    }

    #[test]
    fn knight_attacks() {
        let bb1: BitBoard = mask_knight_attacks(A1);
        let bb2: BitBoard = mask_knight_attacks(E4);
        let bb3: BitBoard = mask_knight_attacks(G6);
        let bb4: BitBoard = mask_knight_attacks(B7);

        assert_eq!(bb1, BitBoard(1128098930098176));
        assert_eq!(bb2, BitBoard(11333767002587136));
        assert_eq!(bb3, BitBoard(687463207072));
        assert_eq!(bb4, BitBoard(84410376));
    }

    #[test]
    fn king_attacks(){
        let bb1: BitBoard = mask_king_attacks(A1);
        let bb2: BitBoard = mask_king_attacks(E4);
        let bb3: BitBoard = mask_king_attacks(H6);
        let bb4: BitBoard = mask_king_attacks(D8);

        assert_eq!(bb1, BitBoard(144959613005987840));
        assert_eq!(bb2, BitBoard(61745389371392));
        assert_eq!(bb3, BitBoard(3225468928));
        assert_eq!(bb4, BitBoard(7188));
    }

    #[test]
    fn bishop_attack(){
        let bb1: BitBoard = mask_bishop_attacks(E4, BitBoard(1161084283129857));
        let bb2: BitBoard = mask_bishop_attacks(B7, BitBoard(35253091631104));

        assert_eq!(bb1, BitBoard(1169881047499265));
        assert_eq!(bb2, BitBoard(68854022149));
    }

    #[test]
    fn rook_attack(){
        let bb1: BitBoard = mask_rook_attacks(A8, BitBoard(1099511627778));
        let bb2: BitBoard = mask_rook_attacks(E4, BitBoard(76561335399223296));

        assert_eq!(bb1, BitBoard(1103823438082));
        assert_eq!(bb2, BitBoard(4521393946365952));
    }
}