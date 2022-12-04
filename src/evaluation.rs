//! # PeSTO's tapered evaluation tables with RofChade piece table values
//! 
//! Interpolates piece value tables over 24 different game phases.
use crate::{
  board_repr::Board,
  square::*,
  piece::*,
  search::MAX_DEPTH,
};

const MG_TABLES: [[Eval; SQUARE_COUNT]; PIECE_COUNT] = [
    [94, 94, 94, 94, 94, 94, 94, 94, 272, 267, 252, 228, 241, 226, 259, 281, 188, 194, 179, 161, 150, 147, 176, 178, 126, 118, 107, 99, 92, 98, 111, 111, 107, 103, 91, 87, 87, 86, 97, 93, 98, 101, 88, 95, 94, 89, 93, 86, 107, 102, 102, 104, 107, 94, 96, 87, 94, 94, 94, 94, 94, 94, 94, 94],
    [94, 94, 94, 94, 94, 94, 94, 94, 107, 102, 102, 104, 107, 94, 96, 87, 98, 101, 88, 95, 94, 89, 93, 86, 107, 103, 91, 87, 87, 86, 97, 93, 126, 118, 107, 99, 92, 98, 111, 111, 188, 194, 179, 161, 150, 147, 176, 178, 272, 267, 252, 228, 241, 226, 259, 281, 94, 94, 94, 94, 94, 94, 94, 94],
    [223, 243, 268, 253, 250, 254, 218, 182, 256, 273, 256, 279, 272, 256, 257, 229, 257, 261, 291, 290, 280, 272, 262, 240, 264, 284, 303, 303, 303, 292, 289, 263, 263, 275, 297, 306, 297, 298, 285, 263, 258, 278, 280, 296, 291, 278, 261, 259, 239, 261, 271, 276, 279, 261, 258, 237, 252, 230, 258, 266, 259, 263, 231, 217],
    [252, 230, 258, 266, 259, 263, 231, 217, 239, 261, 271, 276, 279, 261, 258, 237, 258, 278, 280, 296, 291, 278, 261, 259, 263, 275, 297, 306, 297, 298, 285, 263, 264, 284, 303, 303, 303, 292, 289, 263, 257, 261, 291, 290, 280, 272, 262, 240, 256, 273, 256, 279, 272, 256, 257, 229, 223, 243, 268, 253, 250, 254, 218, 182],
    [283, 276, 286, 289, 290, 288, 280, 273, 289, 293, 304, 285, 294, 284, 293, 283, 299, 289, 297, 296, 295, 303, 297, 301, 294, 306, 309, 306, 311, 307, 300, 299, 291, 300, 310, 316, 304, 307, 294, 288, 285, 294, 305, 307, 310, 300, 290, 282, 283, 279, 290, 296, 301, 288, 282, 270, 274, 288, 274, 292, 288, 281, 292, 280],
    [274, 288, 274, 292, 288, 281, 292, 280, 283, 279, 290, 296, 301, 288, 282, 270, 285, 294, 305, 307, 310, 300, 290, 282, 291, 300, 310, 316, 304, 307, 294, 288, 294, 306, 309, 306, 311, 307, 300, 299, 299, 289, 297, 296, 295, 303, 297, 301, 289, 293, 304, 285, 294, 284, 293, 283, 283, 276, 286, 289, 290, 288, 280, 273],
    [525, 522, 530, 527, 524, 524, 520, 517, 523, 525, 525, 523, 509, 515, 520, 515, 519, 519, 519, 517, 516, 509, 507, 509, 516, 515, 525, 513, 514, 513, 511, 514, 515, 517, 520, 516, 507, 506, 504, 501, 508, 512, 507, 511, 505, 500, 504, 496, 506, 506, 512, 514, 503, 503, 501, 509, 503, 514, 515, 511, 507, 499, 516, 492],
    [503, 514, 515, 511, 507, 499, 516, 492, 506, 506, 512, 514, 503, 503, 501, 509, 508, 512, 507, 511, 505, 500, 504, 496, 515, 517, 520, 516, 507, 506, 504, 501, 516, 515, 525, 513, 514, 513, 511, 514, 519, 519, 519, 517, 516, 509, 507, 509, 523, 525, 525, 523, 509, 515, 520, 515, 525, 522, 530, 527, 524, 524, 520, 517],
    [927, 958, 958, 963, 963, 955, 946, 956, 919, 956, 968, 977, 994, 961, 966, 936, 916, 942, 945, 985, 983, 971, 955, 945, 939, 958, 960, 981, 993, 976, 993, 972, 918, 964, 955, 983, 967, 970, 975, 959, 920, 909, 951, 942, 945, 953, 946, 941, 914, 913, 906, 920, 920, 913, 900, 904, 903, 908, 914, 893, 931, 904, 916, 895],
    [903, 908, 914, 893, 931, 904, 916, 895, 914, 913, 906, 920, 920, 913, 900, 904, 920, 909, 951, 942, 945, 953, 946, 941, 918, 964, 955, 983, 967, 970, 975, 959, 939, 958, 960, 981, 993, 976, 993, 972, 916, 942, 945, 985, 983, 971, 955, 945, 919, 956, 968, 977, 994, 961, 966, 936, 927, 958, 958, 963, 963, 955, 946, 956],
    [11926, 11965, 11982, 11982, 11989, 12015, 12004, 11983, 11988, 12017, 12014, 12017, 12017, 12038, 12023, 12011, 12010, 12017, 12023, 12015, 12020, 12045, 12044, 12013, 11992, 12022, 12024, 12027, 12026, 12033, 12026, 12003, 11982, 11996, 12021, 12024, 12027, 12023, 12009, 11989, 11981, 11997, 12011, 12021, 12023, 12016, 12007, 11991, 11973, 11989, 12004, 12013, 12014, 12004, 11995, 11983, 11947, 11966, 11979, 11989, 11972, 11986, 11976, 11957],
    [11947, 11966, 11979, 11989, 11972, 11986, 11976, 11957, 11973, 11989, 12004, 12013, 12014, 12004, 11995, 11983, 11981, 11997, 12011, 12021, 12023, 12016, 12007, 11991, 11982, 11996, 12021, 12024, 12027, 12023, 12009, 11989, 11992, 12022, 12024, 12027, 12026, 12033, 12026, 12003, 12010, 12017, 12023, 12015, 12020, 12045, 12044, 12013, 11988, 12017, 12014, 12017, 12017, 12038, 12023, 12011, 11926, 11965, 11982, 11982, 11989, 12015, 12004, 11983]
];

const EG_TABLES: [[Eval; SQUARE_COUNT]; PIECE_COUNT] = [
    [82, 82, 82, 82, 82, 82, 82, 82, 180, 216, 143, 177, 150, 208, 116, 71, 76, 89, 108, 113, 147, 138, 107, 62, 68, 95, 88, 103, 105, 94, 99, 59, 55, 80, 77, 94, 99, 88, 92, 57, 56, 78, 78, 72, 85, 85, 115, 70, 47, 81, 62, 59, 67, 106, 120, 60, 82, 82, 82, 82, 82, 82, 82, 82],
    [82, 82, 82, 82, 82, 82, 82, 82, 47, 81, 62, 59, 67, 106, 120, 60, 56, 78, 78, 72, 85, 85, 115, 70, 55, 80, 77, 94, 99, 88, 92, 57, 68, 95, 88, 103, 105, 94, 99, 59, 76, 89, 108, 113, 147, 138, 107, 62, 180, 216, 143, 177, 150, 208, 116, 71, 82, 82, 82, 82, 82, 82, 82, 82],
    [170, 248, 303, 288, 398, 240, 322, 230, 264, 296, 409, 373, 360, 399, 344, 320, 290, 397, 374, 402, 421, 466, 410, 381, 328, 354, 356, 390, 374, 406, 355, 359, 324, 341, 353, 350, 365, 356, 358, 329, 314, 328, 349, 347, 356, 354, 362, 321, 308, 284, 325, 334, 336, 355, 323, 318, 232, 316, 279, 304, 320, 309, 318, 314],
    [232, 316, 279, 304, 320, 309, 318, 314, 308, 284, 325, 334, 336, 355, 323, 318, 314, 328, 349, 347, 356, 354, 362, 321, 324, 341, 353, 350, 365, 356, 358, 329, 328, 354, 356, 390, 374, 406, 355, 359, 290, 397, 374, 402, 421, 466, 410, 381, 264, 296, 409, 373, 360, 399, 344, 320, 170, 248, 303, 288, 398, 240, 322, 230],
    [336, 369, 283, 328, 340, 323, 372, 357, 339, 381, 347, 352, 395, 424, 383, 318, 349, 402, 408, 405, 400, 415, 402, 363, 361, 370, 384, 415, 402, 402, 372, 363, 359, 378, 378, 391, 399, 377, 375, 369, 365, 380, 380, 380, 379, 392, 383, 375, 369, 380, 381, 365, 372, 386, 398, 366, 332, 362, 351, 344, 352, 353, 326, 344],
    [332, 362, 351, 344, 352, 353, 326, 344, 369, 380, 381, 365, 372, 386, 398, 366, 365, 380, 380, 380, 379, 392, 383, 375, 359, 378, 378, 391, 399, 377, 375, 369, 361, 370, 384, 415, 402, 402, 372, 363, 349, 402, 408, 405, 400, 415, 402, 363, 339, 381, 347, 352, 395, 424, 383, 318, 336, 369, 283, 328, 340, 323, 372, 357],
    [509, 519, 509, 528, 540, 486, 508, 520, 504, 509, 535, 539, 557, 544, 503, 521, 472, 496, 503, 513, 494, 522, 538, 493, 453, 466, 484, 503, 501, 512, 469, 457, 441, 451, 465, 476, 486, 470, 483, 454, 432, 452, 461, 460, 480, 477, 472, 444, 433, 461, 457, 468, 476, 488, 471, 406, 458, 464, 478, 494, 493, 484, 440, 451],
    [458, 464, 478, 494, 493, 484, 440, 451, 433, 461, 457, 468, 476, 488, 471, 406, 432, 452, 461, 460, 480, 477, 472, 444, 441, 451, 465, 476, 486, 470, 483, 454, 453, 466, 484, 503, 501, 512, 469, 457, 472, 496, 503, 513, 494, 522, 538, 493, 504, 509, 535, 539, 557, 544, 503, 521, 509, 519, 509, 528, 540, 486, 508, 520],
    [997, 1025, 1054, 1037, 1084, 1069, 1068, 1070, 1001, 986, 1020, 1026, 1009, 1082, 1053, 1079, 1012, 1008, 1032, 1033, 1054, 1081, 1072, 1082, 998, 998, 1009, 1009, 1024, 1042, 1023, 1026, 1016, 999, 1016, 1015, 1023, 1021, 1028, 1022, 1011, 1027, 1014, 1023, 1020, 1027, 1039, 1030, 990, 1017, 1036, 1027, 1033, 1040, 1022, 1026, 1024, 1007, 1016, 1035, 1010, 1000, 994, 975],
    [1024, 1007, 1016, 1035, 1010, 1000, 994, 975, 990, 1017, 1036, 1027, 1033, 1040, 1022, 1026, 1011, 1027, 1014, 1023, 1020, 1027, 1039, 1030, 1016, 999, 1016, 1015, 1023, 1021, 1028, 1022, 998, 998, 1009, 1009, 1024, 1042, 1023, 1026, 1012, 1008, 1032, 1033, 1054, 1081, 1072, 1082, 1001, 986, 1020, 1026, 1009, 1082, 1053, 1079, 997, 1025, 1054, 1037, 1084, 1069, 1068, 1070],
    [11935, 12023, 12016, 11985, 11944, 11966, 12002, 12013, 12029, 11999, 11980, 11993, 11992, 11996, 11962, 11971, 11991, 12024, 12002, 11984, 11980, 12006, 12022, 11978, 11983, 11980, 11988, 11973, 11970, 11975, 11986, 11964, 11951, 11999, 11973, 11961, 11954, 11956, 11967, 11949, 11986, 11986, 11978, 11954, 11956, 11970, 11985, 11973, 12001, 12007, 11992, 11936, 11957, 11984, 12009, 12008, 11985, 12036, 12012, 11946, 12008, 11972, 12024, 12014],
    [11985, 12036, 12012, 11946, 12008, 11972, 12024, 12014, 12001, 12007, 11992, 11936, 11957, 11984, 12009, 12008, 11986, 11986, 11978, 11954, 11956, 11970, 11985, 11973, 11951, 11999, 11973, 11961, 11954, 11956, 11967, 11949, 11983, 11980, 11988, 11973, 11970, 11975, 11986, 11964, 11991, 12024, 12002, 11984, 11980, 12006, 12022, 11978, 12029, 11999, 11980, 11993, 11992, 11996, 11962, 11971, 11935, 12023, 12016, 11985, 11944, 11966, 12002, 12013]
];

const GAME_PHASE_INCREMENT: [Eval; 12] = [ 0, 0, 1, 1, 1, 1, 2, 2, 4, 4, 0, 0];

pub type Eval = i32;
pub const MAX: Eval = 50000;
pub const MIN: Eval = -50000;
pub const MATE_VALUE: Eval = 49000;                             // Value for Mate in 0 ply
pub const MATE_SCORE: Eval = MATE_VALUE - MAX_DEPTH as Eval;    // Minimum mate score possible.

pub fn evaluate(board: &Board) -> Eval {
    let mut mg: [Eval; 2] = [0; 2];
    let mut eg: [Eval; 2] = [0; 2];
    let mut game_phase: Eval = 0;

    for piece in ALL_PIECES {
        for square in board.pieces[piece as usize] {
            mg[piece.color() as usize] += MG_TABLES[piece as usize][square as usize];
            eg[piece.color() as usize] += EG_TABLES[piece as usize][square as usize];
            game_phase += GAME_PHASE_INCREMENT[piece as usize];
        }
    };

    let mg_score: Eval = mg[board.side as usize] - mg[(!board.side) as usize];
    let eg_score: Eval = eg[board.side as usize] - eg[(!board.side) as usize];
    if game_phase > 24 { game_phase = 24 }

    return (mg_score * game_phase + eg_score * (24 - game_phase)) / 24
}