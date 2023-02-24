#!/usr/bin/env python
# Simple script using the chess library to automate format conversion from pgn to marlinflow format
import sys
import random

import chess
import chess.pgn

# Check for correct number of command line arguments
if len(sys.argv) != 3:
    print("Usage: python convert_pgn.py <pgn_file> <out_file>")
    sys.exit(1)

pgn_file = sys.argv[1]
out_file = sys.argv[2]
count = 0
lines = list()

with open(pgn_file, "r") as pgn:
    while True:
        game = chess.pgn.read_game(pgn)
        if game is None:
            break

        result = game.headers["Result"]
        if result == "1-0":
            wdl = "1"
        elif result == "0-1":
            wdl = "0"
        else:
            wdl = "0.5"
        
        board = game.board()
        for node in game.mainline():

            eval = node.comment.split("/")[0]
            if 'M' in eval or "Draw" in eval:
                break

            if eval != "" and not "book" in eval and not board.is_check():
                eval = int(float(eval) * 100)
                if board.turn == chess.BLACK:
                    eval = -eval
                lines.append(board.fen() + " | " + str(eval) + " | " + wdl + "\n")

                count += 1
                if count % 100000 == 0:
                    print(f"Processed {count} FENs")

                # Avoid keeping too many lines in memory (100M ~ 8GB)
                if count >= 100000000:
                    random.shuffle(lines)
                    with open(out_file, "a") as out:
                        out.writelines(lines)
                    lines = list()
                    count = 0
                
            board.push(node.move)

random.shuffle(lines)
with open(out_file, "a") as out:
    out.writelines(lines)
