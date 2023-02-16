# Simple script using the chess library to automate format conversion from pgn to marlinflow format
import sys
import random

import chess
import chess.pgn

# Check for correct number of command line arguments
if len(sys.argv) != 3:
    print("Usage: python3 convert_pgn.py <pgn_file> <out_file>")
    sys.exit(1)

pgn_file = sys.argv[1]
out_file = sys.argv[2]
lines = list()

with open(pgn_file, "r") as pgn:
    while True:
        game = chess.pgn.read_game(pgn)
        if game is None:
            break

        board = game.board()
        result = game.headers["Result"]
        if result == "1-0":
            wdl = "1"
        elif result == "0-1":
            wdl = "0"
        else:
            wdl = "0.5"
        
        for node in game.mainline():
            board.push(node.move)
            
            eval = node.comment.split("/")[0]
            if 'M' in eval or "Draw" in eval:
                break
            if "book" in eval:
                continue

            if node.comment != "" and not board.is_check():
                eval = int(float(eval) * 100)
                lines.append(board.fen() + " | " + str(eval) + " | " + wdl + "\n")

                # Avoid keeping too many lines in memory
                if len(lines) >= 10000000:
                    random.shuffle(lines)
                    with open(out_file, "a") as out:
                        out.writelines(lines)

                    lines = list()

random.shuffle(lines)
with open(out_file, "a") as out:
    out.writelines(lines)
