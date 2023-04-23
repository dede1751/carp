/// Magic numbers and helper functions
/// Process for finding them is documented in https://www.chessprogramming.org/Looking_for_Magics
use super::attacks::*;
use crate::chess::{
    bitboard::*,
    square::*,
};

/// All attacks are stored in the same buffer, each square for bishop/rook gets its slice of this
static mut ATTACKS: [BitBoard; 87988] = [EMPTY_BB; 87988];

// Black magics, contain the magic number and attack table index
#[derive(PartialEq, Eq, PartialOrd, Clone, Copy, Debug, Default)]
struct BlackMagic {
    magic: usize,
    index: usize,
}

macro_rules! M {
    ($m: expr, $i: expr) => {
        BlackMagic {
            magic: $m,
            index: $i,
        }
    };
}

/// Default Bishop black magics found by Niklas Fiekas
#[rustfmt::skip]
const DEFAULT_BISHOP_MAGICS: [BlackMagic; SQUARE_COUNT] = [
    M!(0xA7020080601803D8, 60984), M!(0x13802040400801F1, 66046), M!(0x0A0080181001F60C, 32910),
    M!(0x1840802004238008, 16369), M!(0xC03FE00100000000, 42115), M!(0x24C00BFFFF400000,   835),
    M!(0x0808101F40007F04, 18910), M!(0x100808201EC00080, 25911), M!(0xFFA2FEFFBFEFB7FF, 63301),
    M!(0x083E3EE040080801, 16063), M!(0xC0800080181001F8, 17481), M!(0x0440007FE0031000, 59361),
    M!(0x2010007FFC000000, 18735), M!(0x1079FFE000FF8000, 61249), M!(0x3C0708101F400080, 68938),
    M!(0x080614080FA00040, 61791), M!(0x7FFE7FFF817FCFF9, 21893), M!(0x7FFEBFFFA01027FD, 62068),
    M!(0x53018080C00F4001, 19829), M!(0x407E0001000FFB8A, 26091), M!(0x201FE000FFF80010, 15815),
    M!(0xFFDFEFFFDE39FFEF, 16419), M!(0xCC8808000FBF8002, 59777), M!(0x7FF7FBFFF8203FFF, 16288),
    M!(0x8800013E8300C030, 33235), M!(0x0420009701806018, 15459), M!(0x7FFEFF7F7F01F7FD, 15863),
    M!(0x8700303010C0C006, 75555), M!(0xC800181810606000, 79445), M!(0x20002038001C8010, 15917),
    M!(0x087FF038000FC001,  8512), M!(0x00080C0C00083007, 73069), M!(0x00000080FC82C040, 16078),
    M!(0x000000407E416020, 19168), M!(0x00600203F8008020, 11056), M!(0xD003FEFE04404080, 62544),
    M!(0xA00020C018003088, 80477), M!(0x7FBFFE700BFFE800, 75049), M!(0x107FF00FE4000F90, 32947),
    M!(0x7F8FFFCFF1D007F8, 59172), M!(0x0000004100F88080, 55845), M!(0x00000020807C4040, 61806),
    M!(0x00000041018700C0, 73601), M!(0x0010000080FC4080, 15546), M!(0x1000003C80180030, 45243),
    M!(0xC10000DF80280050, 20333), M!(0xFFFFFFBFEFF80FDC, 33402), M!(0x000000101003F812, 25917),
    M!(0x0800001F40808200, 32875), M!(0x084000101F3FD208,  4639), M!(0x080000000F808081, 17077),
    M!(0x0004000008003F80, 62324), M!(0x08000001001FE040, 18159), M!(0x72DD000040900A00, 61436),
    M!(0xFFFFFEFFBFEFF81D, 57073), M!(0xCD8000200FEBF209, 61025), M!(0x100000101EC10082, 81259),
    M!(0x7FBAFFFFEFE0C02F, 64083), M!(0x7F83FFFFFFF07F7F, 56114), M!(0xFFF1FFFFFFF7FFC1, 57058),
    M!(0x0878040000FFE01F, 58912), M!(0x945E388000801012, 22194), M!(0x0840800080200FDA, 70880),
    M!(0x100000C05F582008, 11140)
];

/// Default Rook black magics found by Niklas Fiekas
#[rustfmt::skip]
const DEFAULT_ROOK_MAGICS: [BlackMagic; SQUARE_COUNT] = [
	M!(0x80280013FF84FFFF, 10890), M!(0x5FFBFEFDFEF67FFF, 50579), M!(0xFFEFFAFFEFFDFFFF, 62020),
    M!(0x003000900300008A, 67322), M!(0x0050028010500023, 80251), M!(0x0020012120A00020, 58503),
    M!(0x0030006000C00030, 51175), M!(0x0058005806B00002, 83130), M!(0x7FBFF7FBFBEAFFFC, 50430),
    M!(0x0000140081050002, 21613), M!(0x0000180043800048, 72625), M!(0x7FFFE800021FFFB8, 80755),
    M!(0xFFFFCFFE7FCFFFAF, 69753), M!(0x00001800C0180060, 26973), M!(0x4F8018005FD00018, 84972),
    M!(0x0000180030620018, 31958), M!(0x00300018010C0003, 69272), M!(0x0003000C0085FFFF, 48372),
    M!(0xFFFDFFF7FBFEFFF7, 65477), M!(0x7FC1FFDFFC001FFF, 43972), M!(0xFFFEFFDFFDFFDFFF, 57154),
    M!(0x7C108007BEFFF81F, 53521), M!(0x20408007BFE00810, 30534), M!(0x0400800558604100, 16548),
    M!(0x0040200010080008, 46407), M!(0x0010020008040004, 11841), M!(0xFFFDFEFFF7FBFFF7, 21112),
    M!(0xFEBF7DFFF8FEFFF9, 44214), M!(0xC00000FFE001FFE0, 57925), M!(0x4AF01F00078007C3, 29574),
    M!(0xBFFBFAFFFB683F7F, 17309), M!(0x0807F67FFA102040, 40143), M!(0x200008E800300030, 64659),
    M!(0x0000008780180018, 70469), M!(0x0000010300180018, 62917), M!(0x4000008180180018, 60997),
    M!(0x008080310005FFFA, 18554), M!(0x4000188100060006, 14385), M!(0xFFFFFF7FFFBFBFFF,     0),
    M!(0x0000802000200040, 38091), M!(0x20000202EC002800, 25122), M!(0xFFFFF9FF7CFFF3FF, 60083),
    M!(0x000000404B801800, 72209), M!(0x2000002FE03FD000, 67875), M!(0xFFFFFF6FFE7FCFFD, 56290),
    M!(0xBFF7EFFFBFC00FFF, 43807), M!(0x000000100800A804, 73365), M!(0x6054000A58005805, 76398),
    M!(0x0829000101150028, 20024), M!(0x00000085008A0014,  9513), M!(0x8000002B00408028, 24324),
    M!(0x4000002040790028, 22996), M!(0x7800002010288028, 23213), M!(0x0000001800E08018, 56002),
    M!(0xA3A80003F3A40048, 22809), M!(0x2003D80000500028, 44545), M!(0xFFFFF37EEFEFDFBE, 36072),
    M!(0x40000280090013C1,  4750), M!(0xBF7FFEFFBFFAF71F,  6014), M!(0xFFFDFFFF777B7D6E, 36054),
    M!(0x48300007E8080C02, 78538), M!(0xAFE0000FFF780402, 28745), M!(0xEE73FFFBFFBB77FE,  8555),
    M!(0x0002000308482882,  1009)
];

// Magics are lazily initialized at startup
pub struct Magics {
    magics: [BlackMagic; SQUARE_COUNT],
    notmasks: BB64,
    shift: usize,
    occupancy_gen: fn(Square) -> BitBoard,
    attack_gen: fn(Square, BitBoard) -> BitBoard,
}

pub static mut BISHOP_MAGICS: Magics = Magics {
    magics: DEFAULT_BISHOP_MAGICS,
    notmasks: EMPTY_BB64,
    shift: 9,
    occupancy_gen: bishop_occupancy,
    attack_gen: mask_bishop_attacks,
};

pub static mut ROOK_MAGICS: Magics = Magics {
    magics: DEFAULT_ROOK_MAGICS,
    notmasks: EMPTY_BB64,
    shift: 12,
    occupancy_gen: rook_occupancy,
    attack_gen: mask_rook_attacks,
};

impl Magics {
    pub fn init(&mut self) {
        for square in ALL_SQUARES {
            let size = 1 << self.shift;
            let occupancies = (self.occupancy_gen)(square);
            self.notmasks[square as usize] = !occupancies;

            for idx in 0..size {
                let blockers = set_occupancy(occupancies, idx);
                let index = self.magic_map(square, blockers);

                unsafe { ATTACKS[index] = (self.attack_gen)(square, blockers) }
            }
        }
    }

    /// Get magic index for the tables given the blocker board and source square
    fn magic_map(&self, square: Square, blockers: BitBoard) -> usize {
        let sq = square as usize;
        let bm = self.magics[sq];

        let mut relevant_occs = (blockers | self.notmasks[sq]).0 as usize;
        relevant_occs = relevant_occs.wrapping_mul(bm.magic);
        relevant_occs >>= 64 - self.shift;

        relevant_occs + bm.index
    }

    /// Get slider attack from square with given blockers
    pub fn attacks(&self, square: Square, blockers: BitBoard) -> BitBoard {
        unsafe { *ATTACKS.get_unchecked(self.magic_map(square, blockers)) }
    }
}
