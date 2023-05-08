# <div align="center"> Carp </div>
<div align="center">

  [![Build][build-badge]][build-link]
  [![License][license-badge]][license-link]
  [![Release][release-badge]][release-link]
  [![Commits][commits-badge]][commits-link]
  
  [![lichess-badge]][lichess-link]
  
</div>

Carp is a UCI-compatible didactic engine written in Rust, using a bitboard-based piece-centric approach.\
The engine is built on top of a conventional Negamax search and uses a simple NNUE evaluation.

The main goal of this project is to learn the basics of both Rust and Chess Programming.

## Rating Lists

| **Version** | [**CCRL 40/15**][ccrl-ltc-link] | [**CCRL BLITZ**][ccrl-blitz-link] | [**MCERL**][mcerl-link] |
|-------------|----------------|-----------|-----------|
| 2.0.0       | 3087           | 3159      | 3117      | 
| 1.3.0       | N/a            | N/a       | 2675      | 
| 1.2.0       | N/a            | N/a       | 2488      | 
| 1.1.0       | N/a            | N/a       | 2164      | 

## Building Carp

Carp can either be build through Cargo with ```cargo run --release```, and the Tools module can be
conditionally compiled with the flag ```--features tools```.

The Cargo build will automatically target the native architecture. To get the various [microarchitecture
levels][arch-link] for the running OS build Carp using the Makefile provided with ```make release```.
These are the executables provided within releases.


## Performance

Move generation is fully legal, inspired by [this article][movegen-link]
and perft(7) on startpos will achieve ~130 MNodes/s locally (a roughly 4x speedup compared to pseudolegal generation).

Newer versions added much more aggressive pruning techniques, which have brought a noticeable speedup in
time to depth and reduced the branching factor greatly. Carp will easily push depth 20-22 with NNUE
enabled, even in complex blitz midgames.

Carp is rigorously tested with SPRT, and has recently joined the [Engine Programming OB instance][ob-link].

## NNUE

Carp uses a 768->(384x2)->1 perspective net trained with MarlinFlow.
It fully relies on its own self-play data for training the network. Data can be generated with the
utilities provided in the Tools module, which can be conditionally compiled by passing the tools 
feature to Cargo.

As of Carp 2.0, NNUE has compltely replaced the old HCE.

## Implemented optimizations

* Fully legal move generation with Fixed Shift Black Magic Bitboards
* Fail-Hard Negamax + Quiescence
* Iterative Deepening with Aspiration Windows
* Move Ordering:
  - MVV-LVA with Static Exchange Evaluation
  - Killers
  - History / Counter Move History / Followup History
* Multithreading with Lazy-SMP
* Lockless Transposition table with newest + highest depth replacement scheme
* Principal Variation Search
* Internal Iterative Reductions
* Late Move Reductions
* Null Move Pruning
* Reverse Futility Pruning
* Mate Distance Pruning
* History Leaf Pruning
* Extended Futility Pruning
* Late Move Pruning
* Delta/SEE/Futility pruning in QS

Of course it is still lacking many optimizations, most notably:

* Pondering for Lichess bot games
* Endgame tablebases
* Various other heuristics (probcut, razoring, search extensions...)
* Continuous net improvements

## Dependencies
None of these are necessary to run the engine, but they are vital for development:

* [Cutechess-Cli](https://github.com/cutechess/cutechess)
* [MarlinFlow](https://github.com/dsekercioglu/marlinflow)
* [OpenBench](https://github.com/AndyGrant/OpenBench)

## Credits
* [CMK's chess engine in C series](https://www.youtube.com/watch?v=QUNP-UjujBM&list=PLmN0neTso3Jxh8ZIylk74JpwfiWNI76Cs)
* [Chess Programming Wiki](https://www.chessprogramming.org/Main_Page)
* Bruce Moreland's [Programming Topics](https://web.archive.org/web/20071026090003/http://www.brucemo.com/compchess/programming/index.htm)
* Cosmo, author of [Viridithas](https://github.com/cosmobobak/viridithas), for a lot of help with NNUE
* Crippa, author of [Svart](https://github.com/crippa1337/svart) for hosting the OB instance Carp is tested with.
* Malu's [Asymptote](https://github.com/malu/asymptote) engine to better understand search heuristics
* The entire chess programming community, for countless awesome resources

[ob-link]:https://chess.swehosting.se/
[arch-link]:https://en.wikipedia.org/wiki/X86-64#Microarchitecture_levels
[ccrl-blitz-link]:https://ccrl.chessdom.com/ccrl/404/cgi/engine_details.cgi?print=Details&each_game=1&eng=Carp%202.0.0%2064-bit#Carp_2_0_0_64-bit
[ccrl-ltc-link]:https://ccrl.chessdom.com/ccrl/4040/cgi/compare_engines.cgi?class=None&only_best_in_class=on&num_best_in_class=1&family=Carp&print=Rating+list&profile_step=50&profile_numbers=1&print=Results+table&print=LOS+table&table_size=100&ct_from_elo=0&ct_to_elo=10000&match_length=30&cross_tables_for_best_versions_only=1&sort_tables=by+rating&diag=0&reference_list=None&recalibrate=no
[mcerl-link]:https://www.chessengeria.com/mcerl
[movegen-link]:https://www.codeproject.com/Articles/5313417/Worlds-Fastest-Bitboard-Chess-Movegenerator

[build-badge]:https://img.shields.io/github/actions/workflow/status/dede1751/carp/rust.yml?branch=master&logo=github&style=for-the-badge
[build-link]:https://github.com/dede1751/carp/actions/workflows/rust.yml
[commits-badge]:https://img.shields.io/github/commits-since/dede1751/carp/latest?style=for-the-badge
[commits-link]:https://github.com/dede1751/carp/commits/master
[release-badge]:https://img.shields.io/github/v/release/dede1751/carp?style=for-the-badge&label=official%20release
[release-link]:https://github.com/dede1751/carp/releases/latest
[license-badge]:https://img.shields.io/github/license/dede1751/carp?style=for-the-badge&label=license&color=success
[license-link]:https://github.com/dede1751/carp/blob/master/LICENSE
[lichess-badge]:https://img.shields.io/badge/Play%20Carp%20-v1.3-yellow?logo=lichess&style=for-the-badge
[lichess-link]:https://lichess.org/@/Carp_Bot
