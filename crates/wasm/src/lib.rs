/// Bindings to use a single-threaded version of Carp in the browser.
/// This is a very minimal version of the engine, meant to be used by my personal website.
/// We rely on a simple callback system where the Rust code calls JS functions to update UI.
///
/// Communication follows UCI standards: https://gist.github.com/DOBRO/2592c6dad754ba67e6dcaec8c90165bf
///
/// # Example Usage (from JS)
/// ```
/// // Initialize engine with 256 MB transposition table.
/// engine = new CarpEngine(256);
///
/// // Perform a perft test. Will call back update_perft_data() with PerftData.
/// engine.perft("startpos", 6);
///
/// // Search the position. Will call update_search_data() with SearchOutput and update_engine_pick() with the best move
/// engine.search("startpos moves e2e4 c7c5", "depth 22");
/// ```
use serde::Serialize;
use wasm_bindgen::prelude::*;

use std::{
    sync::Arc,
    sync::atomic::{AtomicBool, AtomicU64},
};

use engine::clock::{Clock, TimeControl};
use engine::position::Position;
use engine::search_params::{LONGEST_MATE, MATE, MAX_DEPTH};
use engine::syzygy::probe::TB;
use engine::thread::Thread;
use engine::tt::TT;

use web_time::Instant;

#[wasm_bindgen]
unsafe extern "C" {
    fn update_perft_data(s: PerftOutput);
    fn update_search_data(s: SearchOutput);
    fn update_engine_pick(s: &str);
}

#[wasm_bindgen]
#[derive(Default)]
pub struct CarpEngine {
    tt: TT,
    tb: TB, // not actually used by wasm
}

#[wasm_bindgen]
#[derive(Serialize)]
pub struct PerftOutput {
    pub time: u64,
    pub nodes: u64,
    pub nps: f64,
    #[wasm_bindgen(getter_with_clone)]
    pub mov: Option<String>,
}

#[wasm_bindgen]
#[derive(Clone, Copy, Serialize)]
pub enum ScoreType {
    Cp,
    Mate,
    Mated,
}

#[wasm_bindgen]
#[derive(Clone, Copy, Serialize)]
pub struct Score {
    pub val: i32,
    pub w: i32,
    pub d: i32,
    pub l: i32,
}

#[wasm_bindgen]
#[derive(Serialize)]
pub struct SearchOutput {
    pub time: u64,
    pub nodes: u64,
    pub nps: f64,
    pub depth: usize,
    pub score_type: ScoreType,
    pub score: Score,
    #[wasm_bindgen(getter_with_clone)]
    pub pv: String,
}

impl Score {
    pub fn new(val: i32, w: i32, d: i32, l: i32) -> Self {
        Self { val, w, d, l }
    }
}

// Serialize the PerftOutput and SearchOutput structs to JS objects.
#[wasm_bindgen]
impl PerftOutput {
    #[wasm_bindgen(js_name = asObject)]
    pub fn as_object(&self) -> Result<JsValue, JsValue> {
        serde_wasm_bindgen::to_value(self).map_err(|e| e.into())
    }
}

#[wasm_bindgen]
impl SearchOutput {
    #[wasm_bindgen(js_name = asObject)]
    pub fn as_object(&self) -> Result<JsValue, JsValue> {
        serde_wasm_bindgen::to_value(self).map_err(|e| e.into())
    }
}

impl From<&Thread> for SearchOutput {
    fn from(t: &Thread) -> Self {
        // Score logic
        let (score_type, score) = if t.eval.abs() >= LONGEST_MATE {
            let moves_to_mate = (MATE - t.eval.abs() + 1) / 2;

            if t.eval > 0 {
                (ScoreType::Mate, Score::new(moves_to_mate, 1000, 0, 0))
            } else {
                (ScoreType::Mated, Score::new(moves_to_mate, 0, 0, 1000))
            }
        } else {
            let norm_eval = t.normalize_cp_eval();
            let (w, d, l) = t.wdl_model(norm_eval);
            (ScoreType::Cp, Score::new(norm_eval, w, d, l))
        };

        let time = t.clock.elapsed().as_millis().max(1) as u64;
        let nodes = t.clock.global_nodes();
        let nps = (1000.0 * nodes as f64) / (time as f64);

        SearchOutput {
            time,
            nodes,
            nps,
            depth: t.depth,
            score_type,
            score,
            pv: t.pv.to_string(),
        }
    }
}

#[wasm_bindgen]
impl CarpEngine {
    /// Create a new instance of the Carp chess engine.
    ///
    /// # Arguments
    ///
    /// * `size_mb` - The size of the transposition table in megabytes.
    #[wasm_bindgen(constructor)]
    pub fn new(size_mb: usize) -> Self {
        let mut engine = Self::default();
        engine.tt.resize(size_mb);
        engine
    }

    /// Run the PERFT test in `BULK` mode.
    ///
    /// # Arguments
    ///
    /// * `pos_str` - UCI-formatted position string.
    /// * `depth`   - The perft search depth (recommend 1-8).
    pub fn perft(&mut self, pos_str: &str, depth: usize) {
        let position: Position = pos_str.parse().unwrap();
        let board = position.board;

        // Duplicate code from chess::Board::perft with hooks to js.
        let move_list = board.gen_moves::<true>();
        let mut total_nodes = 0;
        let mut total_time = 0;

        for i in 0..move_list.len() {
            let m = move_list.moves[i];
            let move_start = Instant::now();
            let root = board.make_move(m);
            let nodes = root.perft_driver::<true>(depth - 1); // had to make this 'pub'
            let time = move_start.elapsed().as_millis() as u64;
            total_nodes += nodes;
            total_time += time;

            update_perft_data(PerftOutput {
                time,
                nodes,
                nps: (1000.0 * nodes as f64) / (time as f64),
                mov: Some(m.to_string()),
            });
        }

        update_perft_data(PerftOutput {
            time: total_time,
            nodes: total_nodes,
            nps: (1000.0 * total_nodes as f64) / (total_time as f64),
            mov: None,
        });
    }

    /// Search the position for the best move.
    ///
    /// # Arguments
    ///
    /// * `pos_str` - UCI-formatted position string.
    /// * `tc_str`  - UCI-formatted time control string.
    pub fn search(&mut self, pos_str: &str, tc_str: &str) {
        let mut pos: Position = pos_str.parse().unwrap();
        let tc: TimeControl = tc_str.parse().unwrap();

        // Boilerplate init for the search.
        let global_stop = Arc::new(AtomicBool::new(false));
        let global_nodes = Arc::new(AtomicU64::new(0));
        let clock = Clock::new(global_stop, global_nodes, tc, pos.white_to_move());
        let mut t = Thread::new(clock);
        let mut best_move = t.best_move();
        t.clear_for_search(pos.ply(), pos.board.halfmoves);

        // Duplicate code from Postion::iterative_search with hook to js.
        while t.depth < MAX_DEPTH && t.clock.start_search(t.depth + 1, t.nodes, best_move) {
            let eval = pos.aspiration_window(&mut t, &self.tt, self.tb); // had to make this 'pub'
            if t.stop {
                break;
            }

            // Update thread data after a search finishes.
            best_move = t.best_move();
            t.eval = eval;
            t.depth += 1;
            update_search_data(SearchOutput::from(&t));
        }

        update_engine_pick(best_move.to_string().as_str());
    }
}
