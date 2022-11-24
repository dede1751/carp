//! # Find magic numbers through random guesses
//! 
//! Process is documented in https://www.chessprogramming.org/Looking_for_Magics

use std::cell::RefCell;

use crate::bitboard::*;
use crate::square::{ Square, Rank, File, ALL_SQUARES };
use crate::tables::attacks::{
    mask_bishop_attacks,
    mask_rook_attacks,
};

pub const DEFAULT_BISHOP_MAGICS: BB64 = [
    BitBoard(18018831494946945),    BitBoard(5683392850989056), BitBoard(1154048864819347976),
    BitBoard(1130506259411456),     BitBoard(9295994850753466656), BitBoard(722274213532008961),
    BitBoard(144679242373791746),   BitBoard(2819852240160768), BitBoard(72066458926448704),
    BitBoard(79238992794112),       BitBoard(1134698215018542), BitBoard(44092287352833),
    BitBoard(13836474536570134528), BitBoard(35768558952715),       BitBoard(9223409424612344080),
    BitBoard(612885924355252480),   BitBoard(5208413037808118789),  BitBoard(1155245891784015936),
    BitBoard(2252075903829120),    BitBoard(9225624940543279114),   BitBoard(1189513612367104000),
    BitBoard(9512306104749523472), BitBoard(576742365927198723),    BitBoard(4539885961085440),
    BitBoard(2308103606194341892), BitBoard(13862397412175380998),  BitBoard(333415950299702305),
    BitBoard(90076392745664544),   BitBoard(2306410358388064771),   BitBoard(288378294993293312),
    BitBoard(74345757197010958),   BitBoard(2306408175647080720),   BitBoard(73201120490840092),
    BitBoard(9800396855820955656), BitBoard(11529779164388327745),  BitBoard(577166640917512320),
    BitBoard(9800397946725861384), BitBoard(14060563500683528704),  BitBoard(40675471674966276),
    BitBoard(649081871889609226),  BitBoard(144695739342752131),    BitBoard(1154065580883052548),
    BitBoard(1152939184974204944), BitBoard(8079468246723600416),   BitBoard(144124160329646145),
    BitBoard(8163913195585792),    BitBoard(585477299839697040),    BitBoard(596735817053896832),
    BitBoard(1126621814723152),    BitBoard(226064005973413888),    BitBoard(4611796245609973762),
    BitBoard(2305851806414143616), BitBoard(225338878252549120),    BitBoard(6920132705669689392),
    BitBoard(1612323861716860928), BitBoard(2410131644024840),      BitBoard(149467612894740997),
    BitBoard(72057946359738912),   BitBoard(3307863148545),         BitBoard(9288682823518209),
    BitBoard(8725210120),          BitBoard(9259423391126393344),   BitBoard(13835137325615579664),
    BitBoard(1153488869854019624),
];

pub const DEFAULT_ROOK_MAGICS: BB64 = [
    BitBoard(9979994641325359136),  BitBoard(90072129987412032),   BitBoard(180170925814149121),
    BitBoard(72066458867205152),    BitBoard(144117387368072224),  BitBoard(216203568472981512),
    BitBoard(9547631759814820096),  BitBoard(2341881152152807680), BitBoard(140740040605696),
    BitBoard(2316046545841029184),  BitBoard(72198468973629440),   BitBoard(81205565149155328),
    BitBoard(146508277415412736),   BitBoard(1441574144763625600), BitBoard(144959621596184832),
    BitBoard(576742228899270912),   BitBoard(36033470048378880),   BitBoard(72198881818984448),
    BitBoard(1301692025185255936),  BitBoard(90217678106527746),   BitBoard(324684134750365696),
    BitBoard(9265030608319430912),  BitBoard(577591050391142672),  BitBoard(10135298189295745),
    BitBoard(72127964931719168),    BitBoard(2323857549994496000), BitBoard(563233422344228),
    BitBoard(27305413499159048),    BitBoard(11259035584037888),   BitBoard(2201171001472),
    BitBoard(9078684693565976),     BitBoard(581684600791172),     BitBoard(141012643086368),
    BitBoard(9367522478059946048),  BitBoard(9152472371171329),    BitBoard(180214388500734464),
    BitBoard(288249067933272064),   BitBoard(71056073229328),      BitBoard(19796108253360),
    BitBoard(153158682589268036),   BitBoard(4611756387818831872), BitBoard(162147213133225986),
    BitBoard(5769111398076449024),  BitBoard(4503633988190336),    BitBoard(281509354340356),
    BitBoard(15141950770229837952), BitBoard(4706263844262248449), BitBoard(1152923982962491444),
    BitBoard(18014537030570112),    BitBoard(36345525091434816),   BitBoard(4503737075241088),
    BitBoard(288521747001517312),   BitBoard(1443403719252381952), BitBoard(576601506971779200),
    BitBoard(3458765683159483392),  BitBoard(2306124495217363200), BitBoard(4702039830173876225),
    BitBoard(19581379746529409),    BitBoard(362612750088163587),  BitBoard(2310628118748237825),
    BitBoard(281629729755171),      BitBoard(72339120558835829),   BitBoard(11529232780022513828),
    BitBoard(9223382004467269670),
];

/// Returns (bishop, rook) magic number arrays
pub fn generate_magics() -> (BB64, BB64) {
    let mut magics = (EMPTY_BB64, EMPTY_BB64);
    let generator = Rng::new(CMK_STATE);

    for square in ALL_SQUARES {
        magics.0[square.index()] = find_magic(Piece::Bishop(square), &generator);
        magics.1[square.index()] = find_magic(Piece::Rook(square), &generator);
    };

    magics
}

const CMK_STATE: u32 = 1804289383;

/// Consistent RNG through xor shift
struct Rng {
    state: RefCell<u32>,
}

impl Rng {
    pub fn new(state: u32) -> Rng {
        Rng { state: RefCell::new(state) }
    }

    fn rand_u32(&self) -> u32 {
        let mut num = self.state.borrow_mut();
    
        *num ^= *num << 13;
        *num ^= *num >> 17;
        *num ^= *num << 5;

        *num
    }

    fn rand_u64(&self) -> BitBoard {
        let (a, b, c, d) = (
            (self.rand_u32() & 0xFFFF) as u64,  // slice first 16 bits
            (self.rand_u32() & 0xFFFF) as u64,
            (self.rand_u32() & 0xFFFF) as u64,
            (self.rand_u32() & 0xFFFF) as u64);
        
        BitBoard(a | b << 16 | c << 32 | d << 48)
    }

    fn rand_low_bit(&self) -> BitBoard {
        self.rand_u64() & self.rand_u64() & self.rand_u64()
    }
}


/// Mask relevant bishop occupancy bits
pub fn bishop_occupancy(square: Square) -> BitBoard {
    ALL_SQUARES.iter()
               .filter(|tgt| { 
        let (tgt_file, tgt_rank) = tgt.coords();
        let dist: (i8, i8) = square.dist(*tgt);

        tgt_file != File::A     && tgt_file != File::H      &&
        tgt_rank != Rank::First && tgt_rank != Rank::Eight  && // not edges
        dist.0.abs() == dist.1.abs()                        && // diagonal
        **tgt != square
    })
               .fold(EMPTY_BB, |mask, square| { mask ^ square.to_board() })
}

/// Mask relevant rook occupancy bits
pub fn rook_occupancy(square: Square) -> BitBoard {
    ALL_SQUARES.iter()
               .filter(|tgt| { 
        let (tgt_file, tgt_rank) = tgt.coords();
        let dist: (i8, i8) = square.dist(*tgt);

        ((dist.0 == 0 && tgt_rank != Rank::First && tgt_rank != Rank::Eight ) || // same file
        (dist.1 == 0 && tgt_file != File::A && tgt_file != File::H )) &&         // same rank
        **tgt != square
    })
               .fold(EMPTY_BB, |mask, square| { mask ^ square.to_board() })
}

/// Mask index only onto the set bits of the board.
/// 
///     index = 0                        -->  all occupancy bits will be unset
///     index = 2^mask.count_bits() - 1  -->  all occupancy bits will be set
/// 
///     Anything between has some bits set, some unset, and covers all possible combinations
///     of 0s and 1s on the set squares of mask
pub fn set_occupancy(mask: BitBoard, index: usize) -> BitBoard {
    IntoIterator::into_iter(mask)
        .enumerate()
        .filter(|(count, _)| { index & (1 << count) != 0 })
        .fold(EMPTY_BB, |mask, (_, square)| { mask ^ square.to_board() })
}

/// Get magic index for the tables given the blocker board and magic number
/// 
///     - Blockers must already be masked with the relevant occupancies of the square
///     - Bits are the relevant occupancy bits of the square
#[inline]
pub fn magic_map(blockers: BitBoard , magic: BitBoard, bits: u32) -> usize {
    ((blockers * magic).0 >> (64 - bits)) as usize
}

/// piece enum used only for readability
enum Piece{
    Bishop(Square),
    Rook(Square),
}
const ATTEMPTS: u64 = 100000000;
const UPPER_BYTE: BitBoard = BitBoard(0xFF00000000000000);

/// Finds magic bitboard for the given piece on the given square.
/// 
/// Brute force guesses until we find a magic that bijectively maps all possible attack maps from
/// the square. First successful magic is returned.
fn find_magic(piece: Piece, generator: &Rng) -> BitBoard {
    let (mask, square): (BitBoard, Square) = match piece {
        Piece::Bishop(square) => (bishop_occupancy(square), square),
        Piece::Rook(square)   => (rook_occupancy(square), square)
    };

    // generate all possible blockers for square
    let bits: u32 = mask.count_bits();
    let max_index: usize = 1 << bits;
    let occupancies= (0..max_index).map(|x| { 
        set_occupancy(mask, x) });
    
    // get attack mask for each blocker configuration (occupancy, attacks)
    let attacks: Vec<(BitBoard, BitBoard)> = match piece {
        Piece::Bishop(_) => occupancies.map(|x| { (x, mask_bishop_attacks(square, x)) })
                                       .collect(),
        Piece::Rook(_)   => occupancies.map(|x| { (x, mask_rook_attacks(square, x)) })
                                       .collect(),
    };
    
    for _ in 0..ATTEMPTS {
        let magic: BitBoard = generator.rand_low_bit();
        
        // we need the mask magic mapping to have at least 6 bits in the upper byte
        if ((mask * magic) & UPPER_BYTE).count_bits() >= 6 {
            let mut used: [BitBoard; 4096] = [EMPTY_BB; 4096]; // 2^12 max occupancies
            let mut found: bool = true;
            
            // try using magic to map all attack masks
            for (occupancy, attack) in &attacks {
                let magic_index = magic_map(*occupancy, magic, bits);
                
                if used[magic_index] == EMPTY_BB {
                    used[magic_index] = *attack;
                } else if used[magic_index] != *attack {
                    found = false;
                    break;
                }
            }

            if found { return magic }
        }
    }
    
    println!("FAILED!");
    EMPTY_BB
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn bishop_occs(){
        let bb1: BitBoard = bishop_occupancy(Square::A2);
        let bb2: BitBoard = bishop_occupancy(Square::D8);
        let bb3: BitBoard = bishop_occupancy(Square::H1);
        let bb4: BitBoard = bishop_occupancy(Square::E4);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);
        
        assert_eq!(bb1, BitBoard(2216338399232));
        assert_eq!(bb2, BitBoard(1075975168));
        assert_eq!(bb3, BitBoard(18049651735527936));
        assert_eq!(bb4, BitBoard(19184279556981248));
    }

    #[test]
    fn rook_occs(){
        let bb1: BitBoard = rook_occupancy(Square::A8);
        let bb2: BitBoard = rook_occupancy(Square::B7);
        let bb3: BitBoard = rook_occupancy(Square::H2);
        let bb4: BitBoard = rook_occupancy(Square::E4);

        println!("{}\n{}\n{}\n{}\n", bb1, bb2, bb3, bb4);

        assert_eq!(bb1, BitBoard(282578800148862));
        assert_eq!(bb2, BitBoard(565157600328704));
        assert_eq!(bb3, BitBoard(35607136465616896));
        assert_eq!(bb4, BitBoard(4521664529305600));
    }

    #[test]
    fn magics() {
        let magic = find_magic(Piece::Bishop(Square::E4), &Rng::new(CMK_STATE));

        assert_ne!(magic, EMPTY_BB); // if magic is 0, we didn't find it

        let occs = bishop_occupancy(Square::E4);
        let bits: usize = 9;
 
        let mut indices: Vec<usize> = (0..(1 << bits))
                .map(|index| {
            let blockers = set_occupancy(occs, index);
            println!("{} {}", occs, blockers);
            magic_map(blockers, magic, bits as u32)
        })
                .collect();

        indices.sort();
        assert_eq!(indices, (0..(1 << bits)).collect::<Vec<usize>>()) // mapping is bijective
    }
}