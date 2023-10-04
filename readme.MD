<div align="center">

  ![logo](carp_logo.png)
</div>

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

| **Version** | [**CCRL 40/15**][ccrl-ltc-link] | [**CCRL BLITZ**][ccrl-blitz-link] | [**CEGT 40/4**][cegt-link] | [**MCERL**][mcerl-link] |
|-------------|----------------|-----------|-----------|-----------|
| 3.0.1       | 3423           | 3501      | N/a       | N/a       | 
| 2.0.0       | 3087           | 3159      | 2980      | 3117      | 
| 1.3.0       | N/a            | N/a       | N/a       | 2675      | 
| 1.2.0       | N/a            | N/a       | N/a       | 2488      | 
| 1.1.0       | N/a            | N/a       | N/a       | 2164      | 

Carp is rigorously tested with SPRT, and is part of the [SWEHosting OB instance][ob-link].

## Project Structure & Building Carp

The project is structured in 3 main Cargo Workspaces: **Chess** is the game backend used by the engine,
**Engine** is the UCI engine and **Tools** is an executable with developer tools. The first two
crates are designed to be dependency-less, although Syzygy Tablebase support requires interfacing with
[Fathom][fathom-link] (not compiled by default).

Building Carp only requires [Cargo][cargo-link]. Carp is currently tested on *Rust Stable v1.70*, so that is
the recommended version, although it could possibly work (without guarantees) on earlier releases.

For development, Carp should be built through Cargo with ```cargo run --release --package engine```.

To compile Carp for maximum performance, the use of [PGO][pgo-link] is highly recommended. Depending on the CPU,
it has shown to be up to 60 elo over a standard Cargo build. To do this, either run ```make native``` to only
build an executable targeting the machine's specific architecture, or build all available [microarchitecture
levels][arch-link] for the running OS through ```make release```.

**NOTE**: the PGO build requires having *llvm-profdata*, which should be included with LLVM or CLANG.

## NNUE

Carp uses a (768->1024)x2->1 perspective net trained with Bullet.
It fully relies on its own self-play data for training the network. Data can be generated with the
utilities provided in the Tools module. To build for datagen, simply run ```make datagen``` which
also performs PGO.

As of Carp 2.0, NNUE has compltely replaced the old HCE.

## Implemented optimizations

* Fully legal move generation (~435 MNodes/s on a 7950x) with Fixed Shift Black Magic Bitboards
* Fail-Hard Negamax + Quiescence
* Iterative Deepening with Aspiration Windows
* Move Ordering with a staged sorter:
  - MVV + Capture History with Threshold Static Exchange Evaluation
  - Quiet History
  - Continuation Histories
* Multithreading with Lazy-SMP
* Lockless Transposition table with aging
* Principal Variation Search
* Internal Iterative Reductions
* Late Move Reductions
* Null Move Pruning
* Reverse Futility Pruning
* Mate Distance Pruning
* History Leaf Pruning
* Extended Futility Pruning
* Late Move Pruning
* Singular Extensions
* Syzygy Endgame Tablebases

## Dependencies
None of these are strictly necessary to run the engine, but they are vital for development:

* [Fathom][fathom-link]
* [Cutechess-Cli](https://github.com/cutechess/cutechess)
* [Bullet](https://github.com/jw1912/bullet)
* [MarlinFlow](https://github.com/dsekercioglu/marlinflow)
* [OpenBench](https://github.com/AndyGrant/OpenBench)
* [Stockfish's WDL model](https://github.com/official-stockfish/WDL_model)

## Credits
* [CMK's chess engine in C series](https://www.youtube.com/watch?v=QUNP-UjujBM&list=PLmN0neTso3Jxh8ZIylk74JpwfiWNI76Cs)
* [Chess Programming Wiki](https://www.chessprogramming.org/Main_Page)
* Bruce Moreland's [Programming Topics](https://web.archive.org/web/20071026090003/http://www.brucemo.com/compchess/programming/index.htm)
* Cosmo, author of [Viridithas](https://github.com/cosmobobak/viridithas), for a lot of help with NNUE
* Crippa, author of [Svart](https://github.com/crippa1337/svart) for hosting the OB instance Carp is tested with.
* Johan, owner of [SWEHosting](https://swehosting.se/en/) for donating CPU time to our instance.
* Malu's [Asymptote](https://github.com/malu/asymptote) engine to better understand search heuristics
* The entire chess programming community, for countless awesome resources

[ob-link]:https://chess.swehosting.se/
[pgo-link]:https://en.wikipedia.org/wiki/Profile-guided_optimization
[cargo-link]:https://www.rust-lang.org/tools/install
[arch-link]:https://en.wikipedia.org/wiki/X86-64#Microarchitecture_levels
[ccrl-blitz-link]:https://ccrl.chessdom.com/ccrl/404/cgi/compare_engines.cgi?family=Carp&print=Rating+list&print=Results+table&print=LOS+table&print=Ponder+hit+table&print=Eval+difference+table&print=Comopp+gamenum+table&print=Overlap+table&print=Score+with+common+opponents
[ccrl-ltc-link]:https://ccrl.chessdom.com/ccrl/4040/cgi/compare_engines.cgi?family=Carp&print=Rating+list&print=Results+table&print=LOS+table&print=Ponder+hit+table&print=Eval+difference+table&print=Comopp+gamenum+table&print=Overlap+table&print=Score+with+common+opponents
[cegt-link]:http://www.cegt.net/40_4_Ratinglist/40_4_single/rangliste.html
[mcerl-link]:https://www.chessengeria.eu/mcerl
[movegen-link]:https://www.codeproject.com/Articles/5313417/Worlds-Fastest-Bitboard-Chess-Movegenerator
[fathom-link]:https://github.com/jdart1/Fathom

[build-badge]:https://img.shields.io/github/actions/workflow/status/dede1751/carp/build.yml?branch=main&logo=github&style=for-the-badge
[build-link]:https://github.com/dede1751/carp/actions/workflows/build.yml
[commits-badge]:https://img.shields.io/github/commits-since/dede1751/carp/latest?style=for-the-badge
[commits-link]:https://github.com/dede1751/carp/commits/main
[release-badge]:https://img.shields.io/github/v/release/dede1751/carp?style=for-the-badge&label=official%20release
[release-link]:https://github.com/dede1751/carp/releases/latest
[license-badge]:https://img.shields.io/github/license/dede1751/carp?style=for-the-badge&label=license&color=success
[license-link]:https://github.com/dede1751/carp/blob/main/LICENSE
[lichess-badge]:https://img.shields.io/badge/Play%20Carp%20-v3.0-yellow?logo=lichess&style=for-the-badge
[lichess-link]:https://lichess.org/@/Carp_Bot
