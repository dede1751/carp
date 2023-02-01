# Carp
![example workflow](https://github.com/dede1751/carp/actions/workflows/rust.yml/badge.svg)

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

Version 1.2 adds much more aggressive pruning techniques, which have brought a noticeable speedup in
time to depth and reduced the branching factor greatly. Now Carp can easily push to depths 16-17 in
blitz midgames, hitting much softer depth walls.

To benchmark the engine I've also added a test directory with a simple shell script running cutechess
cli. The script allows to run gauntlet tests against engines in the test/engines folder or sprt tests
against older versions of Carp. Engines must be manually added to the shell script.
I still need to properly run the tests, but at first glance it seems that the new version has improved
tremendously just by pruning much more aggressively.

The engine is quite fun to play against when limiting the depth to reasonable values, since
it is relatively dumb positionally: provided it does not see some crazy computer tactic a good
human player should be able to hold out and enjoy a good game.

## Implemented optimizations

* Plain magic bitboards for move precalculation
* Fully legal move generation
* Tapered Evaluation from PeSTO + various optimizations (doubled/passed/isolated pawns, mobility, king safety)
* MVV-LVA with Static Exchange Evaluation + Killer/History quiet move ordering
* Fail-Hard Negamax + Quiescence
* Multithreading with Lazy-SMP
* Iterative Deepening with Aspiration Windows
* Lockless Transposition table with newest + highest depth replacement scheme
* Principal Variation Search
* Null Move Pruning
* Late Move Reductions
* Mate Distance Pruning
* Delta/SEE/Futility pruning in QS

Of course it is still lacking many optimizations, most notably:

* Pondering for Lichess bot games
* Opening book/Endgame tablebase (although UCI let's you do this in the gui)
* Various other heuristics (rfp, lmp, razoring, search extensions...)
* Further improvement to evaluation
