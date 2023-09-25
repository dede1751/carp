
The cooking is complete!
Carp 3.0 irons out a lot of the oddities in the previous release, though still maintaining a lot of
its weird quirks (it is, quite possibly, the strongest Fail Hard engine around). The progtests should
speak for themselves:

## STC (8+0.08)

```
ELO   | 438.76 +- 18.13 (95%)
CONF  | 8.0+0.08s Threads=1 Hash=16MB
GAMES | N: 3024 W: 2609 L: 33 D: 382
```

## LTC (60+0.6)

```
ELO   | 364.09 +- 24.95 (95%)
CONF  | 60.0+0.60s Threads=1 Hash=128MB
GAMES | N: 1000 W: 787 L: 6 D: 207
```

## Changes Overview

* Network improvements: moved to a (768->1024)x2->1 network trained on 3.2B positions
* Countless search improvements and bug fixes.
* Support for Syzygy endgame tablebases (disabled by default as of v3.0.1)
* Major project structural refactor.
* Massively improved chess backend.
<details>
<summary>

#### PR List

</summary>

* Carp0013 by @dede1751 in https://github.com/dede1751/carp/pull/9
* Compact moves by @dede1751 in https://github.com/dede1751/carp/pull/10
* Use 32b eval internally by @dede1751 in https://github.com/dede1751/carp/pull/11
* Nodetm by @dede1751 in https://github.com/dede1751/carp/pull/12
* Use more of the time increment by @dede1751 in https://github.com/dede1751/carp/pull/13
* TT null move by @dede1751 in https://github.com/dede1751/carp/pull/14
* TT static eval by @dede1751 in https://github.com/dede1751/carp/pull/15
* Update best_move on Fail High by @dede1751 in https://github.com/dede1751/carp/pull/16
* Proper handling of possible mates in QS by @dede1751 in https://github.com/dede1751/carp/pull/17
* Remove Delta Pruning in QS by @dede1751 in https://github.com/dede1751/carp/pull/18
* Refactor carp::chess crate by @dede1751 in https://github.com/dede1751/carp/pull/19
* Better threat generation for legal movegen by @dede1751 in https://github.com/dede1751/carp/pull/20
* Fix SEE rejecting equal exchanges. by @dede1751 in https://github.com/dede1751/carp/pull/22
* QS TT move by @dede1751 in https://github.com/dede1751/carp/pull/23
* Material Scale by @dede1751 in https://github.com/dede1751/carp/pull/24
* Singular Extensions by @dede1751 in https://github.com/dede1751/carp/pull/25
* Refactor search by @dede1751 in https://github.com/dede1751/carp/pull/26
* PGO by @dede1751 in https://github.com/dede1751/carp/pull/27
* SEE pruning by @dede1751 in https://github.com/dede1751/carp/pull/28
* Fix conthist by @dede1751 in https://github.com/dede1751/carp/pull/29
* Fix hist score by @dede1751 in https://github.com/dede1751/carp/pull/30
* Implement a MovePicker for staged sorting by @dede1751 in https://github.com/dede1751/carp/pull/31
* Fix MovePick scores by @dede1751 in https://github.com/dede1751/carp/pull/32
* Thread refactor by @dede1751 in https://github.com/dede1751/carp/pull/42
* Keep histories between moves by @dede1751 in https://github.com/dede1751/carp/pull/43
* Shift killers back between moves by @dede1751 in https://github.com/dede1751/carp/pull/44
* PV Table by @dede1751 in https://github.com/dede1751/carp/pull/45
* TT Replacement bug fix + policy change by @dede1751 in https://github.com/dede1751/carp/pull/46
* always set global stop flag after main thread stops by @Ciekce in https://github.com/dede1751/carp/pull/47
* Fix repetition detection by @dede1751 in https://github.com/dede1751/carp/pull/48
* Use a smaller max value for standard histories by @dede1751 in https://github.com/dede1751/carp/pull/49
* Refactor codebase with workspaces by @dede1751 in https://github.com/dede1751/carp/pull/50
* QS Draws by @dede1751 in https://github.com/dede1751/carp/pull/51
* Release CI by @dede1751 in https://github.com/dede1751/carp/pull/52
* Build file by @dede1751 in https://github.com/dede1751/carp/pull/53
* Use 8 BBs in Board by @dede1751 in https://github.com/dede1751/carp/pull/54
* Stop saving temporary static eval entries by @dede1751 in https://github.com/dede1751/carp/pull/55
* Syzygy by @dede1751 in https://github.com/dede1751/carp/pull/56
* Refactor Movegen by @dede1751 in https://github.com/dede1751/carp/pull/57
* QS promos by @dede1751 in https://github.com/dede1751/carp/pull/58
* Caphist by @dede1751 in https://github.com/dede1751/carp/pull/59
* Sane Replacement by @dede1751 in https://github.com/dede1751/carp/pull/61
* Remove killers by @dede1751 in https://github.com/dede1751/carp/pull/62
* Fix evals by @dede1751 in https://github.com/dede1751/carp/pull/63
* Carp0019 by @dede1751 in https://github.com/dede1751/carp/pull/64

</details>

## Binaries

Binaries are released following the [microarchitecture levels](https://en.wikipedia.org/wiki/X86-64#Microarchitecture_levels). It is recommended to at least use V3. As for MacOS, I was unable to build a V3 binary using Github CI, so it is recommended to build locally, as is explained in the ReadME. If possible, a native build is always recommended.

## Special Thanks

Carp development wouldn't be possible without the hardware contributed by the amazing people in the
[SWEHosting](https://chess.swehosting.se/index/) OpenBench instance: @Adam-Kulju, @Alex2262, @archishou, @Ciekce, @cosmobobak, @crippa1337, @GediminasMasaitis, @jhonnold, @jw1912, @lucametehau, @raklaptudirm, @PGG106, @spamdrew128, @uwuplant and most importantly @JouanDeag.

## New Contributors
* @Ciekce made their first contribution in https://github.com/dede1751/carp/pull/47

**Full Changelog**: https://github.com/dede1751/carp/compare/v.2.0.0...v3.0.1