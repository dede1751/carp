# Carp

Carp is a UCI-compatible didactic engine written in Rust, using a bitboard-based piece-centric approach. The main goal of this project is to learn the basics of both Rust and Chess Programming.

Main sources for this project are [CMK's Chess engine in C series](https://www.youtube.com/watch?v=QUNP-UjujBM&list=PLmN0neTso3Jxh8ZIylk74JpwfiWNI76Cs), 
[Jordanbray's move generator](https://github.com/jordanbray/chess), [Malu's Asymptote engine](https://github.com/malu/asymptote)
and most importantly the [chess programming wiki](https://www.chessprogramming.org/Main_Page) and
[Bruce Moreland's Programming Topics](https://web.archive.org/web/20071026090003/http://www.brucemo.com/compchess/programming/index.htm)
(hosted on the wayback machine).

*Code layout/package structure/documentation is temporary for the early stage of development*

*UCI functionality is currently limited to fixed depth search, and both 50MR and threefold repetition are yet to be implemented*

## Performance

Standard perft is available as a test. Locally the move generator performs better than CMK's C engine,
while being 8-9 times slower than the fastest generators around. Understandable given
that it does not bulk count horizon moves, nor keep track of pinned/checking pieces in attack generation.
I will improve generation once done with the actual engine.

As it stands, on my local (rather weak) machine, Carp can iteratively search Kiwipete to depth 10 in roughly
half a second. The engine is quite fun to play against when limiting the depth to reasonable values, since
it is still positionally "dumb", although it is yet to lose against a human player.

## Implemented optimizations

* Plain magic bitboards for move precalculation
* Tapered Evaluation
* MVV-LVA + Killer/History move ordering
* Fail-soft Negamax + Quiescence
* Iterative Deepening with Aspiration Windows
* Transposition table with newest + highest depth replacement scheme
* Principal Variation Search
* Null Move Pruning
* Late Move Reduction
* Mate Distance Pruning
* Delta Pruning

Of course it is still lacking many optimizations, most notably:
* Opening book/Endgame tablebase
* Proper time controls
* Various other heuristics
