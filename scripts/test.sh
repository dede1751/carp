#!/usr/bin/env bash

showHelp() {
cat << EOF  
Usage: ./test -g <rounds> -s <elo-margin> [-h]
Test Carp engine using Cutechess CLI

-h                  Display help

-g                  Run a gauntlet with the given number of rounds against selected engines 

-s                  Run a SPRT test against another engine with the given elo margin

EOF
}
[ $# -eq 0 ] && showHelp 
while getopts ":hg:s:" arg; do
  case $arg in
    # Add engines to this command for the gauntlet
    g)rounds=${OPTARG}
      cutechess-cli \
        -concurrency 1 \
        -tournament gauntlet -rounds $rounds -games 2 -repeat -ratinginterval 1 -recover \
        -engine cmd="./../target/release/carp" name="Carp DEV" proto=uci \
        -each tc=inf/8+0.08 option.Hash=256 option.Threads=1 \
        -openings file=books/UHO_8moves.pgn format=pgn order=random plies=6 \
        -resign movecount=3 score=500 \
        -draw movenumber=50 movecount=5 score=20 \
        -pgnout "gauntlet.pgn"
      ;;
    s)elo=${OPTARG}
      cutechess-cli \
        -concurrency 2 \
        -engine cmd="engines/carp_TTQ_NOPV" name="Carp DEV" proto=uci \
        -engine cmd="engines/carp_CONTROL" name="Carp CONTROL" proto=uci \
        -each tc=inf/8+0.08 option.Hash=256 option.Threads=1 \
        -games 2 -rounds 2500 -repeat 2 -maxmoves 200 \
        -openings file=books/UHO_8moves.pgn format=pgn order=random plies=6 \
        -sprt elo0=0 elo1=$elo alpha=0.05 beta=0.05 \
        -resign movecount=3 score=500 \
        -draw movenumber=50 movecount=5 score=20 \
        -ratinginterval 10 \
        -pgnout "sprt.pgn"
      ;;
    h | *) showHelp
      exit 0
      ;;
  esac
done
