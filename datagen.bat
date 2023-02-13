@echo off
set temp_file=temp.pgn
set data_file=data.txt

rem Generate the game data
cutechess-cli -engine cmd=target/release/carp -engine cmd=target/release/carp ^
    -each proto=uci option.Hash=32 depth=7 tc=inf ^
    -rounds 1 -concurrency 2 ^
    -openings file=UHO_8moves.pgn format=pgn order=random plies=6 ^
    -pgnout %temp_file%

rem Start the venv
python -m venv myenv
myenv\Scripts\activate.bat

rem Convert game data to marlinflow format and shuffle the result
pip install chess
python convert_pgn.py %temp_file% %data_file%
del %temp_file%

rem Check if the marlinflow directory exists, if not clone into it (this is my own fork)
if not exist marlinflow\ (
  git clone https://github.com/dede1751/marlinflow.git
)

rem Build marlinflow, the parser, and make necessary directories
cd marlinflow
cargo build --release

cd parse
cargo build --release

cd ..
mkdir trainer\data trainer\nn trainer\run 2>nul
move target\release\libparse.so trainer\libparse.so

shuf ..\%data_file% > target\release\%data_file%
del ..\%data_file%

rem Run the data compression util
cd target\release
marlinflow-utils.exe txt-to-data %data_file% -o ..\..\trainer\data\data.bin

rem Install python modules for trainer
cd ..\..\trainer
pip install -r requirements.txt

python main.py --data-root data ^
    --train-id carp0000 ^
    --lr 1e-2 ^
    --epochs 30 ^
    --batch-size 16384 ^
    --wdl 0.1 ^
    --scale 400 ^
    --save-epochs 1
