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

## Performance

Move generation is fully legal, inspired by [this article](https://www.codeproject.com/Articles/5313417/Worlds-Fastest-Bitboard-Chess-Movegenerator)
and perft(7) on startpos will achieve ~130 MNodes/s locally (a roughly 4x speedup compared to pseudolegal generation).

Newer versions added much more aggressive pruning techniques, which have brought a noticeable speedup in
time to depth and reduced the branching factor greatly. Carp will easily push depth 20-22 with NNUE
enabled, even in complex blitz midgames.

## NNUE

Carp uses a 768->(256x2)->1 perspective net trained with MarlinFlow.
It fully relies on its own self-play data for training the network, and a datagen script (for windows, as its
my more capable machine) for fen generation. All utilities for training the network are found in the Scripts
directory. As of Carp 2.0, NNUE has compltely replaced the old HCE.

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
* Opening book (although UCI let's you do this in the gui)
* Endgame tablebases
* Various other heuristics (probcut, razoring, search extensions...)
* Continuous net improvements

## Dependencies
None of these are necessary to run the engine, but they are vital for development:

* [Cutechess-Cli](https://github.com/cutechess/cutechess)
* [MarlinFlow](https://github.com/dsekercioglu/marlinflow)

## Credits
* [CMK's chess engine in C series](https://www.youtube.com/watch?v=QUNP-UjujBM&list=PLmN0neTso3Jxh8ZIylk74JpwfiWNI76Cs)
* [Chess Programming Wiki](https://www.chessprogramming.org/Main_Page)
* Bruce Moreland's [Programming Topics](https://web.archive.org/web/20071026090003/http://www.brucemo.com/compchess/programming/index.htm)
* Cosmo, author of [Viridithas](https://github.com/cosmobobak/viridithas), for a lot of help with NNUE
* Malu's [Asymptote](https://github.com/malu/asymptote) engine to better understand search heuristics
* The entire chess programming community, for countless awesome resources

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
