/// Data generator for NNUE training
/// Code is mostly taken from Svart and Viri, credits to the respective authors.
use super::*;
use std::{
    fs::File,
    io::{stdout, BufWriter, Write},
    path::{Path, PathBuf},
    sync::atomic::{AtomicBool, AtomicU64, Ordering},
    sync::Arc,
    time::Instant,
};

use clap::Args;
use carp::{clock::*, position::*, search_params::*, thread::*, tt::*};

/// Generate training data through self-play, defaulting to depth 8 searches.
#[derive(Args)]
pub struct DatagenOptions {
    /// Number of games to run.
    #[arg(long, short = 'g', required = true)]
    games: usize,

    /// Number of threads to run parallel games on.
    #[arg(long, short = 't', required = true)]
    threads: usize,

    /// Limit searches to 'x' nodes.
    #[arg(long, short = 'n', conflicts_with = "depth")]
    nodes: Option<u64>,

    /// Limit searches to 'x' plies.
    #[arg(long, short = 'd')]
    depth: Option<usize>,
}

static STOP_FLAG: AtomicBool = AtomicBool::new(false);
static FENS: AtomicU64 = AtomicU64::new(0);
static WHITE_WINS: AtomicU64 = AtomicU64::new(0);
static BLACK_WINS: AtomicU64 = AtomicU64::new(0);
static WHITE_WIN_ADJ: AtomicU64 = AtomicU64::new(0);
static BLACK_WIN_ADJ: AtomicU64 = AtomicU64::new(0);
static DRAWS: AtomicU64 = AtomicU64::new(0);
static DRAW_ADJ: AtomicU64 = AtomicU64::new(0);

/// Dispatch the datagen threads
pub fn run_datagen(options: &DatagenOptions) {
    ctrlc::set_handler(move || {
        STOP_FLAG.store(true, Ordering::SeqCst);
        println!("Stopping generation...");
    })
    .expect("Failed to set CTRL+C handler.");

    let run_id = format!("run_{}", chrono::Local::now().format("%d-%m-%Y_%H-%M-%S"));
    println!("Data is being saved to {WHITE}data/{run_id}{DEFAULT}");

    let data_dir = PathBuf::from("data").join(run_id);
    std::fs::create_dir_all(&data_dir).unwrap();

    if options.games % options.threads != 0 {
        println!("{ORANGE}WARNING: {DEFAULT}The number of games is not divisible by the number of threads!");
    }

    let games_per_thread = (options.games / options.threads).max(1);
    std::thread::scope(|s| {
        for id in 0..options.threads {
            let path = &data_dir;
            let mut tc = TimeControl::FixedDepth(8);

            if let Some(nodes) = options.nodes {
                tc = TimeControl::FixedNodes(nodes);
            } else if let Some(depth) = options.depth {
                tc = TimeControl::FixedDepth(depth);
            }

            s.spawn(move || {
                datagen_thread(id, games_per_thread, tc, path);
            });
        }
    });
}

/// Run a single datagen thread
/// Each thread will play the given number of games at the given time control, and save the results
/// to a file named after its id.
/// Each game starts with 12 random moves.
fn datagen_thread(id: usize, games: usize, tc: TimeControl, path: &Path) {
    let rng = fastrand::Rng::new();

    let mut position;
    let mut game_buffer: Vec<(Eval, String)> = Vec::new();
    let mut tt = TT::default();

    let mut output_file = File::create(path.join(format!("thread_{id}.txt"))).unwrap();
    let mut output_buffer = BufWriter::new(&mut output_file);

    let timer = Instant::now();

    'main: for games_played in 0..games {
        // Main thread logging
        if id == 0 && games_played != 0 && games_played % 64 == 0 {
            let fens = FENS.load(Ordering::Relaxed);
            let elapsed = timer.elapsed().as_secs_f64();
            let fens_per_sec = fens as f64 / elapsed;

            let ww = WHITE_WINS.load(Ordering::Relaxed);
            let wwa = WHITE_WIN_ADJ.load(Ordering::Relaxed);
            let bw = BLACK_WINS.load(Ordering::Relaxed);
            let bwa = BLACK_WIN_ADJ.load(Ordering::Relaxed);
            let dr = DRAWS.load(Ordering::Relaxed);
            let dra = DRAW_ADJ.load(Ordering::Relaxed);

            let tot_games = ww + wwa + bw + bwa + dr + dra;
            let percentage = (games_played as f64 / games as f64) * 100.0;

            let time_per_game = elapsed / games_played as f64;
            let etr = (games - games_played) as f64 * time_per_game;
            let ecd = chrono::Local::now()
                .checked_add_signed(chrono::Duration::seconds(etr as i64))
                .unwrap()
                .format("%d/%m/%Y %H:%M:%S");

            println!("\n O {GREEN}Generated {DEFAULT}{fens} FENs [{fens_per_sec:.2} FEN/s]");
            println!(" |-> {GREEN}Time per game: {DEFAULT}{time_per_game:.2}s.");
            println!(" |-> In total: {tot_games} [{percentage:.2}%] games are done.");
            println!(" |-> Full Games    --   W: {ww: >8}, B: {bw: >8}, D: {dr: >8}");
            println!(" |-> Adjudications --   W: {wwa: >8}, B: {bwa: >8}, D: {dra: >8}");
            println!(" *-> Elapsed time: {elapsed:.2}s. {RED}ETR: {etr:.2}s on {ecd}{DEFAULT}");

            stdout().flush().unwrap();
        }

        // Reset everything from previous game
        output_buffer.flush().unwrap();
        position = Position::default();

        // Start each game at a random position, skip if randomly stumble into a game over
        // We randomize the starting side to avoid biasing the data
        for _ in 0..rng.usize(8..=9) {
            let move_list = position.board.gen_moves::<true>();

            if move_list.is_empty() || position.is_draw(position.board.halfmoves) {
                continue 'main;
            }

            let m = move_list.moves[rng.usize(..move_list.len())];
            position.push_move(m);
        }

        // Avoid positions that are too unbalanced
        tt.clear();
        let mut thread = Thread::fixed_depth(10);
        position.iterative_search::<false>(&mut thread, &tt);
        if thread.eval.abs() >= 1000 {
            continue 'main;
        }

        // Play out the game
        let mut win_adj_counter = 0;
        let mut draw_adj_counter = 0;

        let game_result = loop {
            let result = position.check_result();
            if result != GameResult::Ongoing {
                break result;
            }

            tt.increment_age();
            thread.advance_ply(1, position.board.halfmoves);
            thread.clock = Clock::new(
                Arc::new(AtomicBool::new(false)),
                Arc::new(AtomicU64::new(0)),
                tc.clone(),
                position.white_to_move(),
            );

            position.iterative_search::<false>(&mut thread, &tt);

            // filter noisy positions
            if !position.king_in_check()
                && thread.eval.abs() < MATE_IN_PLY
                && thread.best_move().get_type().is_quiet()
                && position.ply() > 16
            {
                // Always report scores from white's perspective
                let eval = if position.white_to_move() {
                    thread.eval
                } else {
                    -thread.eval
                };

                game_buffer.push((eval, position.board.to_fen()));
            }

            // Increment adjudication counters
            let abs_eval = thread.eval.abs();
            if abs_eval >= 2000 {
                win_adj_counter += 1;
                draw_adj_counter = 0;
            } else if abs_eval <= 5 {
                draw_adj_counter += 1;
                win_adj_counter = 0;
            } else {
                win_adj_counter = 0;
                draw_adj_counter = 0;
            }

            // Win adjudication after 4 moves at +-2000 centipawns
            if win_adj_counter >= 4 {
                let result = if position.white_to_move() {
                    GameResult::WhiteWin(ADJ)
                } else {
                    GameResult::BlackWin(ADJ)
                };
                break result;
            }
            // Draw adjudication after 12 moves at +-5 centipawns
            if draw_adj_counter >= 12 {
                break GameResult::Draw(ADJ);
            }

            position.push_move(thread.best_move());
        };

        // Always report wins from white's perspective
        let result_output = match game_result {
            GameResult::Draw(adj) => {
                if adj {
                    DRAW_ADJ.fetch_add(1, Ordering::Relaxed);
                } else {
                    DRAWS.fetch_add(1, Ordering::Relaxed);
                }
                "0.5".to_string()
            }
            GameResult::WhiteWin(adj) => {
                if adj {
                    WHITE_WIN_ADJ.fetch_add(1, Ordering::Relaxed);
                } else {
                    WHITE_WINS.fetch_add(1, Ordering::Relaxed);
                }
                "1".to_string()
            }
            GameResult::BlackWin(adj) => {
                if adj {
                    BLACK_WIN_ADJ.fetch_add(1, Ordering::Relaxed);
                } else {
                    BLACK_WINS.fetch_add(1, Ordering::Relaxed);
                }
                "0".to_string()
            }
            _ => unreachable!(),
        };

        // Write the result
        FENS.fetch_add(game_buffer.len() as u64, Ordering::Relaxed);
        for (score, fen) in game_buffer.drain(..) {
            writeln!(output_buffer, "{fen} | {score} | {result_output}").unwrap();
        }

        // Safely abort with CTRLC handler since otherwise
        // our files could get truncated and the data get lost.
        if STOP_FLAG.load(Ordering::SeqCst) {
            break 'main;
        }
    }
}
