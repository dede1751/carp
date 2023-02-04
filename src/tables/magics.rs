/// Magic numbers and helper functions
/// Process for finding them is documented in https://www.chessprogramming.org/Looking_for_Magics
use super::attacks::*;
use crate::bitboard::*;
use crate::square::*;

/// Number of relevant occupancy bits for a bishop on each square
#[rustfmt::skip]
pub const BISHOP_OCCUPANCY_BITS: [u32; SQUARE_COUNT] = [
    6, 5, 5, 5, 5, 5, 5, 6,
    5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 9, 9, 7, 5, 5,
    5, 5, 7, 7, 7, 7, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 5, 5, 5, 5, 5, 5, 6,
];

/// Number of relevant occupancy bits for a rook on each square
#[rustfmt::skip]
pub const ROOK_OCCUPANCY_BITS: [u32; SQUARE_COUNT] = [
    12, 11, 11, 11, 11, 11, 11, 12,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    11, 10, 10, 10, 10, 10, 10, 11,
    12, 11, 11, 11, 11, 11, 11, 12,
];

/// Default bishop magic numbers
#[rustfmt::skip]
pub const DEFAULT_BISHOP_MAGICS: BB64 = [
    BitBoard(18018831494946945),    BitBoard(5683392850989056),     BitBoard(1154048864819347976),
    BitBoard(1130506259411456),     BitBoard(9295994850753466656),  BitBoard(722274213532008961),
    BitBoard(144679242373791746),   BitBoard(2819852240160768),     BitBoard(72066458926448704),
    BitBoard(79238992794112),       BitBoard(1134698215018542),     BitBoard(44092287352833),
    BitBoard(13836474536570134528), BitBoard(35768558952715),       BitBoard(9223409424612344080),
    BitBoard(612885924355252480),   BitBoard(5208413037808118789),  BitBoard(1155245891784015936),
    BitBoard(2252075903829120),     BitBoard(9225624940543279114),  BitBoard(1189513612367104000),
    BitBoard(9512306104749523472),  BitBoard(576742365927198723),   BitBoard(4539885961085440),
    BitBoard(2308103606194341892),  BitBoard(13862397412175380998), BitBoard(333415950299702305),
    BitBoard(90076392745664544),    BitBoard(2306410358388064771),  BitBoard(288378294993293312),
    BitBoard(74345757197010958),    BitBoard(2306408175647080720),  BitBoard(73201120490840092),
    BitBoard(9800396855820955656),  BitBoard(11529779164388327745), BitBoard(577166640917512320),
    BitBoard(9800397946725861384),  BitBoard(14060563500683528704), BitBoard(40675471674966276),
    BitBoard(649081871889609226),   BitBoard(144695739342752131),   BitBoard(1154065580883052548),
    BitBoard(1152939184974204944),  BitBoard(8079468246723600416),  BitBoard(144124160329646145),
    BitBoard(8163913195585792),     BitBoard(585477299839697040),   BitBoard(596735817053896832),
    BitBoard(1126621814723152),     BitBoard(226064005973413888),   BitBoard(4611796245609973762),
    BitBoard(2305851806414143616),  BitBoard(225338878252549120),   BitBoard(6920132705669689392),
    BitBoard(1612323861716860928),  BitBoard(2410131644024840),     BitBoard(149467612894740997),
    BitBoard(72057946359738912),    BitBoard(3307863148545),        BitBoard(9288682823518209),
    BitBoard(8725210120),           BitBoard(9259423391126393344),  BitBoard(13835137325615579664),
    BitBoard(1153488869854019624),
];

/// Default rook magic numbers
#[rustfmt::skip]
pub const DEFAULT_ROOK_MAGICS: BB64 = [
    BitBoard(9979994641325359136),  BitBoard(90072129987412032),    BitBoard(180170925814149121),
    BitBoard(72066458867205152),    BitBoard(144117387368072224),   BitBoard(216203568472981512),
    BitBoard(9547631759814820096),  BitBoard(2341881152152807680),  BitBoard(140740040605696),
    BitBoard(2316046545841029184),  BitBoard(72198468973629440),    BitBoard(81205565149155328),
    BitBoard(146508277415412736),   BitBoard(1441574144763625600),  BitBoard(144959621596184832),
    BitBoard(576742228899270912),   BitBoard(36033470048378880),    BitBoard(72198881818984448),
    BitBoard(1301692025185255936),  BitBoard(90217678106527746),    BitBoard(324684134750365696),
    BitBoard(9265030608319430912),  BitBoard(577591050391142672),   BitBoard(10135298189295745),
    BitBoard(72127964931719168),    BitBoard(2323857549994496000),  BitBoard(563233422344228),
    BitBoard(27305413499159048),    BitBoard(11259035584037888),    BitBoard(2201171001472),
    BitBoard(9078684693565976),     BitBoard(581684600791172),      BitBoard(141012643086368),
    BitBoard(9367522478059946048),  BitBoard(9152472371171329),     BitBoard(180214388500734464),
    BitBoard(288249067933272064),   BitBoard(71056073229328),       BitBoard(19796108253360),
    BitBoard(153158682589268036),   BitBoard(4611756387818831872),  BitBoard(162147213133225986),
    BitBoard(5769111398076449024),  BitBoard(4503633988190336),     BitBoard(281509354340356),
    BitBoard(15141950770229837952), BitBoard(4706263844262248449),  BitBoard(1152923982962491444),
    BitBoard(18014537030570112),    BitBoard(36345525091434816),    BitBoard(4503737075241088),
    BitBoard(288521747001517312),   BitBoard(1443403719252381952),  BitBoard(576601506971779200),
    BitBoard(3458765683159483392),  BitBoard(2306124495217363200),  BitBoard(4702039830173876225),
    BitBoard(19581379746529409),    BitBoard(362612750088163587),   BitBoard(2310628118748237825),
    BitBoard(281629729755171),      BitBoard(72339120558835829),    BitBoard(11529232780022513828),
    BitBoard(9223382004467269670),
];

/// All attacks are stored in the same buffer, each square for bishop/rook gets its slice of this
static mut ATTACKS: [BitBoard; 107648] = [EMPTY_BB; 107648];

/// Magics are lazily initialized at startup
pub struct Magics {
    magics: BB64,
    occupancy: BB64,
    occupancy_bits: [u32; SQUARE_COUNT],
    attacks_offset: usize,
    attacks: [&'static [BitBoard]; SQUARE_COUNT],
    occupancy_gen: fn(Square) -> BitBoard,
    attack_gen: fn(Square, BitBoard) -> BitBoard,
}

pub static mut BISHOP_MAGICS: Magics = Magics {
    magics: DEFAULT_BISHOP_MAGICS,
    occupancy: EMPTY_BB64,
    occupancy_bits: BISHOP_OCCUPANCY_BITS,
    attacks_offset: 0,
    attacks: [&[]; SQUARE_COUNT],
    occupancy_gen: bishop_occupancy,
    attack_gen: mask_bishop_attacks,
};

pub static mut ROOK_MAGICS: Magics = Magics {
    magics: DEFAULT_ROOK_MAGICS,
    occupancy: EMPTY_BB64,
    occupancy_bits: ROOK_OCCUPANCY_BITS,
    attacks_offset: 5248,
    attacks: [&[]; SQUARE_COUNT],
    occupancy_gen: rook_occupancy,
    attack_gen: mask_rook_attacks,
};

impl Magics {
    pub fn init(&mut self) {
        let mut base = self.attacks_offset;

        for square in ALL_SQUARES {
            let size = 1 << self.occupancy_bits[square as usize];
            let mask = (self.occupancy_gen)(square);
            self.occupancy[square as usize] = mask;

            for idx in 0..size {
                let blockers = set_occupancy(mask, idx);
                let index = self.magic_map(square, blockers);

                unsafe { ATTACKS[base + index] = (self.attack_gen)(square, blockers) }
            }

            unsafe { self.attacks[square as usize] = &ATTACKS[base..base + size] }
            base += size;
        }
    }

    /// Get magic index for the tables given the blocker board and source square
    fn magic_map(&self, square: Square, blockers: BitBoard) -> usize {
        let sq = square as usize;
        let map = (blockers & self.occupancy[sq]) * self.magics[sq];

        (map.0 as usize) >> (64 - self.occupancy_bits[sq])
    }

    /// Get slider attack from square with given blockers
    pub fn attacks(&self, square: Square, blockers: BitBoard) -> BitBoard {
        unsafe {
            *self
                .attacks
                .get_unchecked(square as usize)
                .get_unchecked(self.magic_map(square, blockers))
        }
    }
}
