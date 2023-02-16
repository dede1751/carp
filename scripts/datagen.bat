@echo off
set games_file=games.pgn
set data_file=data.txt
set book=books/UHO_8moves.pgn

rem Generate the game data
cd ..
cargo build --release
cd scripts

cutechess-cli -engine cmd=../target/release/carp -engine cmd=../target/release/carp ^
    -each proto=uci option.Hash=128 nodes=10000 tc=inf ^
    -rounds 500000 -concurrency 5 ^
    -openings file=books/UHO_8moves.pgn format=pgn order=random plies=6 ^
    -resign movecount=3 score=500 ^
    -draw movenumber=50 movecount=5 score=20 ^
    -pgnout games.pgn

rem Convert game data to marlinflow format and shuffle the result
python3 convert_pgn.py %games_file% %data_file%

marlinflow-utils txt-to-data %data_file% -o ../marlinflow/trainer/data/data.bin