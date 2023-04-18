/// Data generator for NNUE training
/// Code is mostly taken from Svart and Viri, credits to the respective authors.
use std::{
    fs::File,
    io::{stdout, BufWriter, Write},
    path::{Path, PathBuf},
    sync::atomic::{AtomicBool, AtomicU64, Ordering},
    sync::Arc,
    time::Instant,
};

use super::*;
use crate::clock::*;
use crate::position::*;
use crate::search::*;
use crate::search_params::*;
use crate::tt::TT;

static STOP_FLAG: AtomicBool = AtomicBool::new(false);
static FENS: AtomicU64 = AtomicU64::new(0);
static WHITE_WINS: AtomicU64 = AtomicU64::new(0);
static BLACK_WINS: AtomicU64 = AtomicU64::new(0);
static WHITE_WIN_ADJ: AtomicU64 = AtomicU64::new(0);
static BLACK_WIN_ADJ: AtomicU64 = AtomicU64::new(0);
static DRAWS: AtomicU64 = AtomicU64::new(0);
static DRAW_ADJ: AtomicU64 = AtomicU64::new(0);

pub struct DatagenOptions {
    games: u32,
    threads: u32,
    time_control: TimeControl,
}

fn parse_token(token: &str, suffix: char, err: &str) -> Result<u32, String> {
    token
        .strip_suffix(suffix)
        .ok_or_else(|| format!("{}: {}", err, token))?
        .parse()
        .map_err(|_| format!("{}: {}", err, token))
}

impl std::str::FromStr for DatagenOptions {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.split('-').collect::<Vec<_>>();
        if parts.len() != 3 {
            return Err(format!("Invalid datagen string: {}", s));
        }

        let games = parse_token(parts[0], 'g', "Invalid number of games")?;
        let threads = parse_token(parts[1], 't', "Invalid number of threads")?;

        let limit_type = parts[2]
            .chars()
            .last()
            .ok_or_else(|| format!("Invalid limit: {}", parts[2]))?;
        let limit_value = parse_token(parts[2], limit_type, "Invalid limit")?;

        let time_control = match limit_type {
            'd' => TimeControl::FixedDepth(limit_value as usize),
            'n' => TimeControl::FixedNodes(limit_value as u64),
            _ => return Err(format!("Invalid limit: {}", parts[2])),
        };

        Ok(DatagenOptions {
            games,
            threads,
            time_control,
        })
    }
}

impl std::fmt::Display for DatagenOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let limit = match self.time_control {
            TimeControl::FixedDepth(depth) => format!("{}d", depth),
            TimeControl::FixedNodes(nodes) => format!("{}n", nodes),
            _ => unreachable!(),
        };

        write!(f, "{}g-{}t-{}", self.games, self.threads, limit)
    }
}

/// Dispatch the datagen threads
pub fn run_datagen(options: DatagenOptions) {
    ctrlc::set_handler(move || {
        STOP_FLAG.store(true, Ordering::SeqCst);
        println!("Stopping generation...");
    })
    .expect("Failed to set CTRL+C handler.");

    let run_id = format!(
        "run_{}_{options}",
        chrono::Local::now().format("%d-%m-%Y_%H-%M-%S")
    );
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
            let tc = &options.time_control;

            s.spawn(move || {
                datagen_thread(id, games_per_thread, tc, path);
            });
        }
    });

    // After all threads are finished, merge and dedup all the data into a single file
    match merge::merge(data_dir) {
        Ok(()) => {
            if STOP_FLAG.load(Ordering::SeqCst) {
                println!("\n\t{ORANGE}Data generation stopped prematurely.{DEFAULT}");
            } else {
                println!("\n\t{GREEN}Data generation completed successfully.{DEFAULT}");
            }
        }
        Err(e) => println!("{ORANGE}MERGE ERROR{DEFAULT}: {}", e),
    }
}

/// Run a single datagen thread
/// Each thread will play the given number of games at the given time control, and save the results
/// to a file named after its id.
/// Each game starts with 12 random moves.
fn datagen_thread(id: u32, games: u32, tc: &TimeControl, path: &Path) {
    let rng = fastrand::Rng::new();

    let mut position;
    let mut game_buffer: Vec<(Eval, String)> = vec![];

    let mut output_file = File::create(path.join(format!("thread_{id}.txt"))).unwrap();
    let mut output_buffer = BufWriter::new(&mut output_file);

    let timer = Instant::now();

    'main: for games_played in 0..games {
        // Main thread logging
        if id == 0 && games_played != 0 && games_played % 6 == 0 {
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
        for _ in 0..12 {
            let move_list = position.board.gen_moves::<true>();

            if move_list.is_empty() || position.is_draw() {
                continue 'main;
            }

            let m = move_list.moves[rng.usize(..move_list.len())];
            position.push_move(m);
        }

        // Avoid positions that are too unbalanced
        let clock = Clock::new(
            TimeControl::FixedDepth(8),
            Arc::new(AtomicBool::new(false)),
            true,
        );
        let (_, eval, _) =
            Search::new(position.clone(), clock, &TT::new(16)).iterative_search::<NO_INFO>();
        if eval.abs() >= 1000 {
            continue 'main;
        }

        // Play out the game
        let mut win_adj_counter = 0;
        let mut draw_adj_counter = 0;
        let tt = TT::new(16);

        let game_result = loop {
            let result = position.check_result();
            if result != GameResult::Ongoing {
                break result;
            }

            let clock = Clock::new(
                tc.clone(),
                Arc::new(AtomicBool::new(false)),
                position.white_to_move(),
            );
            let (m, mut eval, _) =
                Search::new(position.clone(), clock, &tt).iterative_search::<NO_INFO>();

            // filter noisy positions
            if !position.king_in_check()
                && !is_mate(eval.abs())
                && !m.is_capture()
                && !m.is_promotion()
            {
                // Always report scores from white's perspective
                eval = if position.white_to_move() {
                    eval
                } else {
                    -eval
                };

                game_buffer.push((eval, position.board.to_fen()));
            }

            // Increment adjudication counters
            let abs_eval = eval.abs();
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

            position.push_move(m);
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
