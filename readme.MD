# Carp
<div align="center">

  [![Build][build-badge]][build-link]
  [![License][license-badge]][license-link]
  [![Release][release-badge]][release-link]
  [![Commits][commits-badge]][commits-link]
  
</div>

Carp is a UCI-compatible didactic engine written in Rust, using a bitboard-based piece-centric approach.
The main goal of this project is to learn the basics of both Rust and Chess Programming.

[Play against Carp on Lichess! (looking to get a raspi for stable hosting)](https://lichess.org/@/Carp_Bot)

Main sources for this project are [CMK's Chess engine in C series](https://www.youtube.com/watch?v=QUNP-UjujBM&list=PLmN0neTso3Jxh8ZIylk74JpwfiWNI76Cs), 
[Jordanbray's move generator](https://github.com/jordanbray/chess), [Malu's Asymptote engine](https://github.com/malu/asymptote)
and most importantly the [chess programming wiki](https://www.chessprogramming.org/Main_Page) and
[Bruce Moreland's Programming Topics](https://web.archive.org/web/20071026090003/http://www.brucemo.com/compchess/programming/index.htm)
(hosted on the wayback machine).

## Performance

Move generation is fully legal, inspired by [this article](https://www.codeproject.com/Articles/5313417/Worlds-Fastest-Bitboard-Chess-Movegenerator)
and perft(7) on startpos will achieve ~130 MNodes/s locally (a roughly 4x speedup compared to pseudolegal generation).

Newer versions added much more aggressive pruning techniques, which have brought a noticeable speedup in
time to depth and reduced the branching factor greatly. Now Carp can easily push to depths 18-20 in
blitz midgames, hitting much softer depth walls.
Version 1.3 includes many search optimizations discussed in the chess programming wiki, leading to
a massive elo increase. Currently I would estimate Carp to be ~2750 CCRL40/15 elo on 4CPU, though I
will wait until done with evaluation to submit it to competitions.

To benchmark the engine I've also added a test directory with a simple shell script running cutechess
cli. The script allows to run gauntlet tests against other engines or SPRT tests against older versions
of Carp. Engines must be manually added to the shell script.

The engine is quite fun to play against when limiting the depth to reasonable values, since
it is relatively dumb positionally: provided it does not see some crazy computer tactic a good
human player should be able to hold out and enjoy a good game.

## Implemented optimizations

* Fully legal move generation using plain magic bitboards
* Tapered Evaluation from PeSTO + various optimizations (doubled/passed/isolated pawns, mobility, king safety)
* MVV-LVA with Static Exchange Evaluation + Killer/History/CMH/FUH
* Fail-Hard Negamax + Quiescence
* Multithreading with Lazy-SMP
* Lockless Transposition table with newest + highest depth replacement scheme
* Iterative Deepening with Aspiration Windows
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
* Further improvement to evaluation/NNUE

I also want to work on OpenBench compliance to simplify testing.

[build-badge]:https://img.shields.io/github/actions/workflow/status/dede1751/carp/rust.yml?branch=master&logo=github&style=for-the-badge
[build-link]:https://github.com/dede1751/carp/actions/workflows/rust.yml
[commits-badge]:https://img.shields.io/github/commits-since/dede1751/carp/latest?style=for-the-badge
[commits-link]:https://github.com/dede1751/carp/commits/master
[release-badge]:https://img.shields.io/github/v/release/dede1751/carp?style=for-the-badge&label=official%20release
[release-link]:https://github.com/dede1751/carp/releases/latest
[license-badge]:https://img.shields.io/github/license/dede1751/carp?style=for-the-badge&label=license&color=success
[license-link]:https://github.com/dede1751/carp/blob/master/LICENSE
