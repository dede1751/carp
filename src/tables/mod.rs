/// Module for initializing various global constants
/// 
/// The module exposes the precalculated attack tables, but also includes functionality to generate
/// new zobrist keys and evaluation tables, to replace the hardcoded ones.
/// 
/// Carp uses plain magic bitboards found through random guessing for slider piece attack tables.
/// RNG for the guessing is built-in and consistent (no need for security), magic values are
/// hardcoded into the engine because there really is no need to change them. The code to generate
/// them remains in the rng module.
/// Move tables are used for attacks only: quiet moves are calculated on the fly by the move
/// generator.

mod attacks;
mod magics;
mod customize;

use attacks::*;
use magics::*;

use crate::bitboard::*;
use crate::square::*;
use crate::piece::Color;

/// Various bitmasks for evaluation
pub const FILE_MASKS: BB64 = [BitBoard(72340172838076673), BitBoard(144680345676153346), BitBoard(289360691352306692), BitBoard(578721382704613384), BitBoard(1157442765409226768), BitBoard(2314885530818453536), BitBoard(4629771061636907072), BitBoard(9259542123273814144), BitBoard(72340172838076673), BitBoard(144680345676153346), BitBoard(289360691352306692), BitBoard(578721382704613384), BitBoard(1157442765409226768), BitBoard(2314885530818453536), BitBoard(4629771061636907072), BitBoard(9259542123273814144), BitBoard(72340172838076673), BitBoard(144680345676153346), BitBoard(289360691352306692), BitBoard(578721382704613384), BitBoard(1157442765409226768), BitBoard(2314885530818453536), BitBoard(4629771061636907072), BitBoard(9259542123273814144), BitBoard(72340172838076673), BitBoard(144680345676153346), BitBoard(289360691352306692), BitBoard(578721382704613384), BitBoard(1157442765409226768), BitBoard(2314885530818453536), BitBoard(4629771061636907072), BitBoard(9259542123273814144), BitBoard(72340172838076673), BitBoard(144680345676153346), BitBoard(289360691352306692), BitBoard(578721382704613384), BitBoard(1157442765409226768), BitBoard(2314885530818453536), BitBoard(4629771061636907072), BitBoard(9259542123273814144), BitBoard(72340172838076673), BitBoard(144680345676153346), BitBoard(289360691352306692), BitBoard(578721382704613384), BitBoard(1157442765409226768), BitBoard(2314885530818453536), BitBoard(4629771061636907072), BitBoard(9259542123273814144), BitBoard(72340172838076673), BitBoard(144680345676153346), BitBoard(289360691352306692), BitBoard(578721382704613384), BitBoard(1157442765409226768), BitBoard(2314885530818453536), BitBoard(4629771061636907072), BitBoard(9259542123273814144), BitBoard(72340172838076673), BitBoard(144680345676153346), BitBoard(289360691352306692), BitBoard(578721382704613384), BitBoard(1157442765409226768), BitBoard(2314885530818453536), BitBoard(4629771061636907072), BitBoard(9259542123273814144)];
pub const RANK_MASKS: BB64 = [BitBoard(255), BitBoard(255), BitBoard(255), BitBoard(255), BitBoard(255), BitBoard(255), BitBoard(255), BitBoard(255), BitBoard(65280), BitBoard(65280), BitBoard(65280), BitBoard(65280), BitBoard(65280), BitBoard(65280), BitBoard(65280), BitBoard(65280), BitBoard(16711680), BitBoard(16711680), BitBoard(16711680), BitBoard(16711680), BitBoard(16711680), BitBoard(16711680), BitBoard(16711680), BitBoard(16711680), BitBoard(4278190080), BitBoard(4278190080), BitBoard(4278190080), BitBoard(4278190080), BitBoard(4278190080), BitBoard(4278190080), BitBoard(4278190080), BitBoard(4278190080), BitBoard(1095216660480), BitBoard(1095216660480), BitBoard(1095216660480), BitBoard(1095216660480), BitBoard(1095216660480), BitBoard(1095216660480), BitBoard(1095216660480), BitBoard(1095216660480), BitBoard(280375465082880), BitBoard(280375465082880), BitBoard(280375465082880), BitBoard(280375465082880), BitBoard(280375465082880), BitBoard(280375465082880), BitBoard(280375465082880), BitBoard(280375465082880), BitBoard(71776119061217280), BitBoard(71776119061217280), BitBoard(71776119061217280), BitBoard(71776119061217280), BitBoard(71776119061217280), BitBoard(71776119061217280), BitBoard(71776119061217280), BitBoard(71776119061217280), BitBoard(18374686479671623680), BitBoard(18374686479671623680), BitBoard(18374686479671623680), BitBoard(18374686479671623680), BitBoard(18374686479671623680), BitBoard(18374686479671623680), BitBoard(18374686479671623680), BitBoard(18374686479671623680)];
pub const ISOLATED_MASKS: BB64 = [BitBoard(144680345676153346), BitBoard(361700864190383365), BitBoard(723401728380766730), BitBoard(1446803456761533460), BitBoard(2893606913523066920), BitBoard(5787213827046133840), BitBoard(11574427654092267680), BitBoard(4629771061636907072), BitBoard(144680345676153346), BitBoard(361700864190383365), BitBoard(723401728380766730), BitBoard(1446803456761533460), BitBoard(2893606913523066920), BitBoard(5787213827046133840), BitBoard(11574427654092267680), BitBoard(4629771061636907072), BitBoard(144680345676153346), BitBoard(361700864190383365), BitBoard(723401728380766730), BitBoard(1446803456761533460), BitBoard(2893606913523066920), BitBoard(5787213827046133840), BitBoard(11574427654092267680), BitBoard(4629771061636907072), BitBoard(144680345676153346), BitBoard(361700864190383365), BitBoard(723401728380766730), BitBoard(1446803456761533460), BitBoard(2893606913523066920), BitBoard(5787213827046133840), BitBoard(11574427654092267680), BitBoard(4629771061636907072), BitBoard(144680345676153346), BitBoard(361700864190383365), BitBoard(723401728380766730), BitBoard(1446803456761533460), BitBoard(2893606913523066920), BitBoard(5787213827046133840), BitBoard(11574427654092267680), BitBoard(4629771061636907072), BitBoard(144680345676153346), BitBoard(361700864190383365), BitBoard(723401728380766730), BitBoard(1446803456761533460), BitBoard(2893606913523066920), BitBoard(5787213827046133840), BitBoard(11574427654092267680), BitBoard(4629771061636907072), BitBoard(144680345676153346), BitBoard(361700864190383365), BitBoard(723401728380766730), BitBoard(1446803456761533460), BitBoard(2893606913523066920), BitBoard(5787213827046133840), BitBoard(11574427654092267680), BitBoard(4629771061636907072), BitBoard(144680345676153346), BitBoard(361700864190383365), BitBoard(723401728380766730), BitBoard(1446803456761533460), BitBoard(2893606913523066920), BitBoard(5787213827046133840), BitBoard(11574427654092267680), BitBoard(4629771061636907072)];
pub const DOUBLED_MASKS: [BB64; 2] = [
    [BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(1), BitBoard(2), BitBoard(4), BitBoard(8), BitBoard(16), BitBoard(32), BitBoard(64), BitBoard(128), BitBoard(257), BitBoard(514), BitBoard(1028), BitBoard(2056), BitBoard(4112), BitBoard(8224), BitBoard(16448), BitBoard(32896), BitBoard(65793), BitBoard(131586), BitBoard(263172), BitBoard(526344), BitBoard(1052688), BitBoard(2105376), BitBoard(4210752), BitBoard(8421504), BitBoard(16843009), BitBoard(33686018), BitBoard(67372036), BitBoard(134744072), BitBoard(269488144), BitBoard(538976288), BitBoard(1077952576), BitBoard(2155905152), BitBoard(4311810305), BitBoard(8623620610), BitBoard(17247241220), BitBoard(34494482440), BitBoard(68988964880), BitBoard(137977929760), BitBoard(275955859520), BitBoard(551911719040), BitBoard(1103823438081), BitBoard(2207646876162), BitBoard(4415293752324), BitBoard(8830587504648), BitBoard(17661175009296), BitBoard(35322350018592), BitBoard(70644700037184), BitBoard(141289400074368), BitBoard(282578800148737), BitBoard(565157600297474), BitBoard(1130315200594948), BitBoard(2260630401189896), BitBoard(4521260802379792), BitBoard(9042521604759584), BitBoard(18085043209519168), BitBoard(36170086419038336)],
    [BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(1), BitBoard(2), BitBoard(4), BitBoard(8), BitBoard(16), BitBoard(32), BitBoard(64), BitBoard(128), BitBoard(257), BitBoard(514), BitBoard(1028), BitBoard(2056), BitBoard(4112), BitBoard(8224), BitBoard(16448), BitBoard(32896), BitBoard(65793), BitBoard(131586), BitBoard(263172), BitBoard(526344), BitBoard(1052688), BitBoard(2105376), BitBoard(4210752), BitBoard(8421504), BitBoard(16843009), BitBoard(33686018), BitBoard(67372036), BitBoard(134744072), BitBoard(269488144), BitBoard(538976288), BitBoard(1077952576), BitBoard(2155905152), BitBoard(4311810305), BitBoard(8623620610), BitBoard(17247241220), BitBoard(34494482440), BitBoard(68988964880), BitBoard(137977929760), BitBoard(275955859520), BitBoard(551911719040), BitBoard(1103823438081), BitBoard(2207646876162), BitBoard(4415293752324), BitBoard(8830587504648), BitBoard(17661175009296), BitBoard(35322350018592), BitBoard(70644700037184), BitBoard(141289400074368), BitBoard(282578800148737), BitBoard(565157600297474), BitBoard(1130315200594948), BitBoard(2260630401189896), BitBoard(4521260802379792), BitBoard(9042521604759584), BitBoard(18085043209519168), BitBoard(36170086419038336)],
];
pub const PASSED_MASKS: [BB64; 2] = [
    [BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(3), BitBoard(7), BitBoard(14), BitBoard(28), BitBoard(56), BitBoard(112), BitBoard(224), BitBoard(192), BitBoard(771), BitBoard(1799), BitBoard(3598), BitBoard(7196), BitBoard(14392), BitBoard(28784), BitBoard(57568), BitBoard(49344), BitBoard(197379), BitBoard(460551), BitBoard(921102), BitBoard(1842204), BitBoard(3684408), BitBoard(7368816), BitBoard(14737632), BitBoard(12632256), BitBoard(50529027), BitBoard(117901063), BitBoard(235802126), BitBoard(471604252), BitBoard(943208504), BitBoard(1886417008), BitBoard(3772834016), BitBoard(3233857728), BitBoard(12935430915), BitBoard(30182672135), BitBoard(60365344270), BitBoard(120730688540), BitBoard(241461377080), BitBoard(482922754160), BitBoard(965845508320), BitBoard(827867578560), BitBoard(3311470314243), BitBoard(7726764066567), BitBoard(15453528133134), BitBoard(30907056266268), BitBoard(61814112532536), BitBoard(123628225065072), BitBoard(247256450130144), BitBoard(211934100111552), BitBoard(847736400446211), BitBoard(1978051601041159), BitBoard(3956103202082318), BitBoard(7912206404164636), BitBoard(15824412808329272), BitBoard(31648825616658544), BitBoard(63297651233317088), BitBoard(54255129628557504)],
    [BitBoard(217020518514230016), BitBoard(506381209866536704), BitBoard(1012762419733073408), BitBoard(2025524839466146816), BitBoard(4051049678932293632), BitBoard(8102099357864587264), BitBoard(16204198715729174528), BitBoard(13889313184910721024), BitBoard(217020518514229248), BitBoard(506381209866534912), BitBoard(1012762419733069824), BitBoard(2025524839466139648), BitBoard(4051049678932279296), BitBoard(8102099357864558592), BitBoard(16204198715729117184), BitBoard(13889313184910671872), BitBoard(217020518514032640), BitBoard(506381209866076160), BitBoard(1012762419732152320), BitBoard(2025524839464304640), BitBoard(4051049678928609280), BitBoard(8102099357857218560), BitBoard(16204198715714437120), BitBoard(13889313184898088960), BitBoard(217020518463700992), BitBoard(506381209748635648), BitBoard(1012762419497271296), BitBoard(2025524838994542592), BitBoard(4051049677989085184), BitBoard(8102099355978170368), BitBoard(16204198711956340736), BitBoard(13889313181676863488), BitBoard(217020505578799104), BitBoard(506381179683864576), BitBoard(1012762359367729152), BitBoard(2025524718735458304), BitBoard(4051049437470916608), BitBoard(8102098874941833216), BitBoard(16204197749883666432), BitBoard(13889312357043142656), BitBoard(217017207043915776), BitBoard(506373483102470144), BitBoard(1012746966204940288), BitBoard(2025493932409880576), BitBoard(4050987864819761152), BitBoard(8101975729639522304), BitBoard(16203951459279044608), BitBoard(13889101250810609664), BitBoard(216172782113783808), BitBoard(504403158265495552), BitBoard(1008806316530991104), BitBoard(2017612633061982208), BitBoard(4035225266123964416), BitBoard(8070450532247928832), BitBoard(16140901064495857664), BitBoard(13835058055282163712), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0), BitBoard(0)],
];

const BISHOP_OCCUPANCY_BITS: [u32; SQUARE_COUNT] = [
    6, 5, 5, 5, 5, 5, 5, 6,
    5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 5, 5, 5, 5, 5, 5, 6,
];
const ROOK_OCCUPANCY_BITS: [u32; SQUARE_COUNT] = [
    12, 11, 11, 11, 11, 11, 11, 12,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    12, 11, 11, 11, 11, 11, 11, 12,
];

/// Precalculated attack tables for all pieces
#[derive(Debug)]
pub struct Tables {
    pawn_attacks  : [BB64; 2],
    knight_attacks: BB64,
    king_attacks  : BB64,

    bishop_magics: BB64,
    bishop_occupancies: BB64,
    bishop_attacks: Vec<Vec<BitBoard>>,

    rook_magics: BB64,
    rook_occupancies: BB64,
    rook_attacks: Vec<Vec<BitBoard>>,
}

impl Default for Tables {
    fn default() -> Self {
        let mut tables = Tables{
            pawn_attacks  : [EMPTY_BB64; 2],
            knight_attacks: EMPTY_BB64,
            king_attacks  : EMPTY_BB64,

            bishop_magics: DEFAULT_BISHOP_MAGICS,
            bishop_occupancies: EMPTY_BB64,
            bishop_attacks    : Vec::new(),

            rook_magics: DEFAULT_ROOK_MAGICS,
            rook_occupancies: EMPTY_BB64,
            rook_attacks    : Vec::new(),
        };

        tables.init_leaper_attacks();
        tables.init_slider_attacks();

        tables
    }
}

/// Table initialization
impl Tables {
    // initializes leaper tables
    fn init_leaper_attacks(&mut self) {
        for square in ALL_SQUARES {
            let rank = square.rank();
            let sq = square as usize;
    
            // pawn attacks are also generated behind the starting rank, needed to be able to check
            // quickly for attacks when the opposite colors pawns are one rank from promotion
            if rank != Rank::Eight {
                self.pawn_attacks[0][sq] = mask_pawn_attacks(square, Color::White);
            }
            if rank != Rank::First {
                self.pawn_attacks[1][sq] = mask_pawn_attacks(square, Color::Black);
            }
            self.knight_attacks[sq] = mask_knight_attacks(square);
            self.king_attacks[sq] = mask_king_attacks(square);
        }
    }

    // initializes slider tables using magics
    fn init_slider_attacks(&mut self) {
        for square in ALL_SQUARES {
            // gen bishop tables and occupancies
            let mask = bishop_occupancy(square);
            let relevant_bits = mask.count_bits();
            let mut occ_map: Vec<BitBoard> = vec!(EMPTY_BB; 1 << relevant_bits);

            for index in 0..(1 << relevant_bits) {
                let blockers = set_occupancy(mask, index);
                let magic_index = magic_map(
                    blockers, self.bishop_magics[square as usize], relevant_bits);
                
                occ_map[magic_index] = mask_bishop_attacks(square, blockers);
            };

            self.bishop_occupancies[square as usize] = mask;
            self.bishop_attacks.push(occ_map);

            // gen rook tables and occupancies
            let mask = rook_occupancy(square);
            let relevant_bits = mask.count_bits();
            let mut occ_map: Vec<BitBoard> = vec!(EMPTY_BB; 1 << relevant_bits);

            for index in 0..(1 << relevant_bits) {
                let blockers = set_occupancy(mask, index);
                let magic_index = magic_map(
                    blockers, self.rook_magics[square as usize], relevant_bits);
                
                occ_map[magic_index] = mask_rook_attacks(square, blockers);
            };

            self.rook_occupancies[square as usize] = mask;
            self.rook_attacks.push(occ_map);
        }
    } 
}

/// Table lookup
impl Tables {
    /// Gets pawn attacks from tables
    #[inline]
    pub fn get_pawn_attack(&self, square: Square, side: Color) -> BitBoard {
        match side {
            Color::White => self.pawn_attacks[0][square as usize],
            Color::Black => self.pawn_attacks[1][square as usize],
        }
    }

    /// Gets knight attacks from tables
    #[inline]
    pub fn get_knight_attack(&self, square: Square) -> BitBoard {
        self.knight_attacks[square as usize]
    }

    /// Gets king attacks from tables
    #[inline]
    pub fn get_king_attack(&self, square: Square) -> BitBoard {
        self.king_attacks[square as usize]
    }

    /// Gets bishop attacks based on the blocker bitboard
    #[inline]
    pub fn get_bishop_attack(&self, square: Square, blockers: BitBoard) -> BitBoard {
        let sq = square as usize;
        let bits = BISHOP_OCCUPANCY_BITS[sq];
        let magic_index = magic_map(
            blockers & self.bishop_occupancies[sq],
            self.bishop_magics[sq],
            bits);

        self.bishop_attacks[sq][magic_index]
    }

    /// Gets rook attacks based on the blocker bitboard
    #[inline]
    pub fn get_rook_attack(&self, square: Square, blockers: BitBoard) -> BitBoard {
        let sq = square as usize;
        let bits = ROOK_OCCUPANCY_BITS[sq];
        let magic_index = magic_map(
            blockers & self.rook_occupancies[sq],
            self.rook_magics[sq],
            bits);

        self.rook_attacks[sq][magic_index]
    }

    /// Gets queen attacks based on the blocker bitboard
    #[inline]
    pub fn get_queen_attack(&self, square: Square, blockers: BitBoard) -> BitBoard {
        self.get_rook_attack(square, blockers) | self.get_bishop_attack(square, blockers)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bishop_attacks(){
        let t = Tables::default();

        let bb1: BitBoard = t.get_bishop_attack(Square::E4, BitBoard(1161084283129857));
        let bb2: BitBoard = t.get_bishop_attack(Square::B7, BitBoard(35253091631104));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1169881047499265));
        assert_eq!(bb2, BitBoard(68854022149));
    }

    #[test]
    fn rook_attacks(){
        let t = Tables::default();

        let bb1: BitBoard = t.get_rook_attack(Square::A8, BitBoard(1099511627778));
        let bb2: BitBoard = t.get_rook_attack(Square::E4, BitBoard(76561335399223296));

        println!("{}\n{}\n", bb1, bb2);

        assert_eq!(bb1, BitBoard(1103823438082));
        assert_eq!(bb2, BitBoard(4521393946365952));
    }
}