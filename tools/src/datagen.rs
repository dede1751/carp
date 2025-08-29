/// Data generator for NNUE training
/// Code is mostly taken from Svart and Viri, credits to the respective authors.
use super::{binpack::ViriformatGame, *};
use std::{
    fs::File,
    io::{stdout, BufWriter, Write},
    path::{Path, PathBuf},
    sync::atomic::{AtomicBool, AtomicU64, Ordering},
    sync::Arc,
    time::Instant,
};

use chess::moves::Move;
use clap::Args;
use engine::{
    clock::{Clock, TimeControl},
    position::{GameResult, Position, ADJ, NO_ADJ},
    search_params::*,
    syzygy::probe::TB,
    thread::Thread,
    tt::TT,
};

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

    /// Path to Syzygy Tablebases, used for search and adjudication.
    #[arg(long, short = 's')]
    syzygy: Option<String>,
}

static STOP_FLAG: AtomicBool = AtomicBool::new(false);
static POSITIONS: AtomicU64 = AtomicU64::new(0);
static WHITE_WINS: AtomicU64 = AtomicU64::new(0);
static BLACK_WINS: AtomicU64 = AtomicU64::new(0);
static WHITE_WIN_ADJ: AtomicU64 = AtomicU64::new(0);
static BLACK_WIN_ADJ: AtomicU64 = AtomicU64::new(0);
static DRAWS: AtomicU64 = AtomicU64::new(0);
static DRAW_ADJ: AtomicU64 = AtomicU64::new(0);

/// Dispatch the datagen threads
pub fn run_datagen(options: DatagenOptions) {
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

    let mut tb = TB::default();
    if let Some(path) = options.syzygy {
        tb.activate(&path, TB::MAX_MEN);
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
                datagen_thread(id, games_per_thread, tc, tb, path);
            });
        }
    });
}

/// Generate a random move in a position, trying to avoid moves that are too bad.
fn gen_random_move(pos: &Position, rng: &fastrand::Rng) -> Option<Move> {
    let move_list = pos.board.gen_moves::<true>();
    if move_list.is_empty() || pos.is_draw(pos.board.halfmoves) {
        return None;
    }

    // try to get a positive see move, otherwise just a random one.
    let mut m = Move::NULL;
    for _ in 0..8 {
        m = move_list.moves[rng.usize(..move_list.len())];
        if pos.board.see(m, 0) {
            return Some(m);
        }
    }
    Some(m) // can never be a null move
}

/// Run a single datagen thread
/// Each thread will play the given number of games at the given time control, and save the results
/// to a file named after its id.
fn datagen_thread(id: usize, games: usize, tc: TimeControl, tb: TB, path: &Path) {
    let mut tt = TT::default();
    let rng = fastrand::Rng::new();

    let mut packed_game = ViriformatGame::new();
    let mut output_file = File::create(path.join(format!("thread_{id}.bin"))).unwrap();
    let mut output_buffer = BufWriter::new(&mut output_file);

    let timer = Instant::now();

    'main: for games_played in 0..games {
        // Main thread logging
        if id == 0 && games_played != 0 && games_played % 64 == 0 {
            let positions = POSITIONS.load(Ordering::Relaxed);
            let elapsed = timer.elapsed().as_secs_f64();
            let pos_per_sec = positions as f64 / elapsed;

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

            println!(
                "\n O {GREEN}Generated {DEFAULT}{positions} positions [{pos_per_sec:.2} POS/s]"
            );
            println!(" |-> {GREEN}Time per game: {DEFAULT}{time_per_game:.2}s.");
            println!(" |-> In total: {tot_games} [{percentage:.2}%] games are done.");
            println!(" |-> Full Games    --   W: {ww: >8}, B: {bw: >8}, D: {dr: >8}");
            println!(" |-> Adjudications --   W: {wwa: >8}, B: {bwa: >8}, D: {dra: >8}");
            println!(" *-> Elapsed time: {elapsed:.2}s. {RED}ETR: {etr:.2}s on {ecd}{DEFAULT}");

            stdout().flush().unwrap();
        }

        // Start each game at a random position, skip if randomly stumble into a game over
        // We randomize the starting side to avoid biasing the data
        let mut position = Position::default();
        for _ in 0..rng.usize(8..=9) {
            if let Some(m) = gen_random_move(&position, &rng) {
                position.push_move(m);
            } else {
                continue 'main;
            }
        }

        // Avoid positions that are too unbalanced with a short search.
        let mut thread = Thread::fixed_depth(10);
        tt.clear();

        position.iterative_search::<false>(&mut thread, &tt, tb);
        if thread.eval.abs() >= 1000 {
            continue 'main;
        }

        // Play out the game
        packed_game.start(&position.board);
        let mut win_adj_counter = 0;
        let mut draw_adj_counter = 0;

        let game_result = loop {
            let result = position.check_result(tb);
            let wtm = position.white_to_move();
            if result != GameResult::Ongoing {
                break result;
            }

            tt.increment_age();
            thread.clear_for_search(position.ply(), position.board.halfmoves);
            thread.clock = Clock::new(
                Arc::new(AtomicBool::new(false)),
                Arc::new(AtomicU64::new(0)),
                tc,
                wtm,
            );

            // Search and always report scores from white's perspective
            position.iterative_search::<false>(&mut thread, &tt, tb);
            let best_move = thread.best_move();
            let eval = if wtm { thread.eval } else { -thread.eval };
            let abs_eval = eval.abs();

            if best_move == Move::NULL {
                println!("{ORANGE}WARNING: {DEFAULT}The engine returned a null move in this position: {}", &position.board);
            }
            packed_game.push_move(best_move, eval);

            // Increment adjudication counters
            if abs_eval >= 2500 {
                // will shortcut if eval jumps from +2500 to -2500, hopefully impossible
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
                let result = if eval > 0 {
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

            // Stop when a mate is found
            if abs_eval >= LONGEST_TB_MATE {
                if eval > 0 {
                    break GameResult::WhiteWin(NO_ADJ);
                } else {
                    break GameResult::BlackWin(NO_ADJ);
                }
            }
            position.push_move(thread.best_move());
        };

        // Always report wins from white's perspective
        match game_result {
            GameResult::Draw(adj) => {
                if adj {
                    DRAW_ADJ.fetch_add(1, Ordering::Relaxed);
                } else {
                    DRAWS.fetch_add(1, Ordering::Relaxed);
                }
            }
            GameResult::WhiteWin(adj) => {
                if adj {
                    WHITE_WIN_ADJ.fetch_add(1, Ordering::Relaxed);
                } else {
                    WHITE_WINS.fetch_add(1, Ordering::Relaxed);
                }
            }
            GameResult::BlackWin(adj) => {
                if adj {
                    BLACK_WIN_ADJ.fetch_add(1, Ordering::Relaxed);
                } else {
                    BLACK_WINS.fetch_add(1, Ordering::Relaxed);
                }
            }
            _ => unreachable!(),
        };

        // Write the result
        POSITIONS.fetch_add(packed_game.len() as u64, Ordering::Relaxed);
        packed_game.set_result(game_result);
        packed_game.write_bytes(&mut output_buffer).unwrap();
        output_buffer.flush().unwrap();

        // Safely abort with CTRLC handler since otherwise
        // our files could get truncated and the data get lost.
        if STOP_FLAG.load(Ordering::SeqCst) {
            break 'main;
        }
    }
}
