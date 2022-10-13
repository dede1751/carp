use std::ops::Not;

pub enum Color { White, Black }
pub use Color::*;

impl Not for Color {
    type Output = Color;

    // get opposite color
    #[inline]
    fn not(self) -> Color {
        match self {
            Black => White,
            White => Black,
        }
    }
}