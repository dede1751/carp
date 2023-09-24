
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
* Support for Syzygy endgame tablebases.
* Major project structural refactor.
* Massively improved chess backend.

## Contributors

Carp development wouldn't be possible without the hardware contributed by the amazing people in the
[SWEHosting](https://chess.swehosting.se/index/) OpenBench instance: @Adam-Kulju, @Alex2262, @archishou,
@Ciekce, @cosmobobak, @crippa1337, @GediminasMasaitis, @jhonnold, @jw1912, @lucametehau, @raklaptudirm,
@spamdrew128, @uwuplant and most importantly @JouanDeag.