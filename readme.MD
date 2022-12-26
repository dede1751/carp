# Carp 1.0.0
![example workflow](https://github.com/dede1751/carp/actions/workflows/rust.yml/badge.svg)

Carp is a UCI-compatible didactic engine written in Rust, using a bitboard-based piece-centric approach.
The main goal of this project is to learn the basics of both Rust and Chess Programming.

Main sources for this project are [CMK's Chess engine in C series](https://www.youtube.com/watch?v=QUNP-UjujBM&list=PLmN0neTso3Jxh8ZIylk74JpwfiWNI76Cs), 
[Jordanbray's move generator](https://github.com/jordanbray/chess), [Malu's Asymptote engine](https://github.com/malu/asymptote)
and most importantly the [chess programming wiki](https://www.chessprogramming.org/Main_Page) and
[Bruce Moreland's Programming Topics](https://web.archive.org/web/20071026090003/http://www.brucemo.com/compchess/programming/index.htm)
(hosted on the wayback machine).

## Performance

Standard perft is available as a UCI command, alongside benchmarking using the Eigenmann Rapid test
set. Locally the move generator performs better than CMK's C engine, while being 8-9 times slower
than the fastest generators around.
Understandable given that it does not bulk count horizon moves, nor keep track of pinned/checking
pieces in attack generation. I will improve generation once done with the actual engine.

As it stands, on my local (rather weak) machine, Carp will usually search to depths 11-13 in fast
time controls during the middle game.
In standard arena games, with the improved evaluation, it seems to handily beat BBC, although
further and more rigorous testing should be conduced.

The engine is quite fun to play against when limiting the depth to reasonable values, since
it is relatively dumb positionally: provided it does not see some crazy computer tactic a good
human player should be able to hold out and enjoy a good game.

## Implemented optimizations

* Plain magic bitboards for move precalculation
* Tapered Evaluation from PeSTO + various optimizations (doubled/passed/isolated pawns, mobility, king safety)
* MVV-LVA + Killer/History move ordering
* Fail-Hard Negamax + Quiescence
* Multithreading with Lazy-SMP
* Iterative Deepening with Aspiration Windows
* Lockless Transposition table with newest + highest depth replacement scheme
* Principal Variation Search
* Null Move Pruning
* Late Move Reduction
* Mate Distance Pruning
* Futility Pruning

Of course it is still lacking many optimizations, most notably:

* Opening book/Endgame tablebase (although UCI let's you do this in the gui)
* Various other heuristics (razoring, see, search extensions...)
* Further improvement to evaluation
