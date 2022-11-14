# Carp

Carp will be a didactic chess engine written in Rust, using a BitBoard-based piece-centric approach,
relying on plain magics for attack precalculation.
The main goal of this project learn the basics of both Rust and Chess Programming
Currently there is no engine/evaluation component, but move generation has been completed, although
I plan on improving its efficiency by adding pin/check maps and other various optimizations.

Main sources for this project are [CMK's Chess engine in C series](https://www.youtube.com/watch?v=QUNP-UjujBM&list=PLmN0neTso3Jxh8ZIylk74JpwfiWNI76Cs)
and [Jordanbray's move generator](https://github.com/jordanbray/chess).

*Code layout/package structure/documentation is temporary for the early stage of development*

## Performance

Standard perft is available in the tests crate. Locally the move generator performs better than its
C counterpart, while being 8-9 times slower than the fastest generators around. Understandable given
that it does not bulk count horizon moves, nor keep track of pinned/checking pieces in attack generation.