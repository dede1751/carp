/// Engine module contains all the logic used by the Carp engine

pub mod position;
pub mod move_sorter;
pub mod tt;
pub mod clock;
pub mod nnue;
pub mod search;
pub mod search_params;
pub mod uci;

pub use uci::UCIController;