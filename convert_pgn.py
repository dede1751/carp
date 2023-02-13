# Simple script using the chess library to automate format conversion from pgn to marlinflow format
import sys
import io

import chess
import chess.pgn

# Check for correct number of command line arguments
if len(sys.argv) != 3:
    print("Usage: python3 convert_pgn.py <pgn_file> <out_file>")
    sys.exit(1)

pgn_file = sys.argv[1]
out_file = sys.argv[2]

with open(pgn_file, "r") as pgn, open(out_file, "w") as out:

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
                out.write(board.fen() + " | " + str(eval) + " | " + wdl + "\n")
