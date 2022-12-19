/// Search the move tree
/// 
///     Negamax:
/// Alpha : lower bound, the lowest score the current player is assured of
/// Beta  : upper bound, the highest score the opposite player is assured of
/// 
/// Acceptable nodes for the current player fall between alpha and beta: a score above alpha is better
/// than anything we've been guaranteed so far. A score above beta is "too good" and the opponent
/// will simply choose another branch of the tree, hence we can "cutoff" or prune the branch.
/// 
///     TT scores and moves:
/// Positions are hashed and saved in a lookup "transposition table" kept throughout the entire game
/// At every node of the search tree we lookup the position in the tt to obtain the best move from
/// previous searches and the evaluation, which can be an upper/lower bound or an exact value. 
/// The tt move is searched first, while the value is only used if it's been searched at an appropriate
/// depth.
/// 
///     Move ordering:
/// For efficient pruning it is important to search the best moves first. This is handled by the
/// move sorter using various static heuristics.
/// 
///     -- Optimizations:
/// 
/// * QUIESCENCE SEARCH
/// Search until a "quiet" position where no captures are possible when reaching the highest depth.
/// Avoids the horizon effect, where the engine throws pieces away because it doesn't see the recapture.
/// Either player can also "stand pat" at any point in the search and just accept the position without
/// being force to recapture.
/// 
/// * FUTILITY/DELTA PRUNING (withing quiescence)
/// During an exchange, if the side to move can't make up the material difference even with the best
/// recapture possible (a full queen) it's probably not worth going through the entire exchange.
/// 
/// * MATE DISTANCE PRUNING
/// When we've already found a mate, ignore lines where any mate is necessarily longer.
/// 
/// * NULL MOVE PRUNING
/// Try making a null move, aka giving your opponent a free shot. If that is still enough to go
/// over beta (which means the null move itself is too good for us) skip searching the moves which
/// will most likely all be better than not doing anything. The main exception is of course Zugzwang.
/// Since a null move can be responded to with a null move, this should be taken care of.
/// 
/// * PRINCIPAL VARIATION SEARCH
/// Assuming good move ordering, the first move is probably the most noteworthy and will tell us
/// what kind of node we are in:
///     PV-NODE   : A node where we improve alpha without going over beta.
///     BETA-NODE : A node where we improve alpha too much and go over beta.
///     ALPHA-NODE: A node where we do not improve alpha.
/// Hence, if the first move was scored between alpha and beta, it's unlikely we will find an even
/// better move that will produce a beta cutoff. From the first move on, we try to "prove" that
/// all other moves are worse by using a much faster null-window search around alpha. We expect
/// this search to fail low (not pass alpha), otherwise we will have to properly search the move
/// 
/// * LATE MOVE REDUCTION
/// Like pvs, this assumes the earliest moves are the best. Later moves will be searched at a lower
/// depth to prove they are worse, otherwise they will be searched again at the proper depth

use std::cmp::{ max, min };
use crate::{
    board::Board,
    tables::Tables,
    moves::*,
    move_order::*,
    evaluation::*,
    clock::Clock,
    tt::*,
    zobrist::ZHash,
};

pub const MAX_DEPTH: u8 = 128; // max depth to search at
const LMR_THRESHOLD: u32 = 4;  // moves to execute before any reduction
const LMR_LOWER_LIMIT: u8 = 3; // stop applying lmr near leaves
const NMP_REDUCTION: u8 = 2;   // null move pruning reduced depth

const ASPIRATION_WINDOW: Eval = 50; // aspiration window width
const ASPIRATION_THRESHOLD: u8 = 4; // depth at which windows are reduced

const FUTILITY_MARGIN: Eval = 1100; // highest queen value possible

pub struct Search<'a>{
    clock: Clock,
    tt: &'a TT,
    sorter: MoveSorter,
    history: Vec<ZHash>,
    tables: &'a Tables,
    nodes: u64,
    stop: bool,
    age: u8,
}

impl <'a> Search<'a>{
    pub fn new(
        history: Vec<ZHash>,
        clock: Clock,
        tt: &'a TT,
        tables: &'a Tables,
    ) -> Search<'a> {
        let age = (history.len() + 1) as u8 ; // age is always at least one
        
        Search {
            history,
            clock,
            tt,
            tables,
            sorter: MoveSorter::new(),
            nodes: 0,
            stop: false,
            age
        }
    }

    /// Iteratively searches the board at increasing depth
    pub fn iterative_search(&mut self, board: Board, print_info: bool) -> (Move, u8) {
        let mut best_move: Move = NULL_MOVE;
        let mut temp_best: Move;
        let mut alpha: Eval = -MATE;
        let mut beta : Eval = MATE;
        let mut eval: Eval = 0;
        let mut depth: u8 = 1;

        // don't search draws
        if self.is_draw(&board) { return (NULL_MOVE, 0); }

        while !self.stop                        &&
            self.clock.start_check(depth)       &&
            !(is_mate(eval) || is_mated(eval))  &&
            depth < MAX_DEPTH
        {
            if depth < ASPIRATION_THRESHOLD {
                // don't apply aspiration windows to shallow searches (< 4 ply deep)
                (eval, temp_best) = self.search_root(&board, alpha, beta, depth);
            } else {
                // reduce window using previous eval
                alpha = eval - ASPIRATION_WINDOW;
                beta  = eval + ASPIRATION_WINDOW;
                (eval, temp_best) = self.search_root(&board, alpha, beta, depth);

                if (eval <= alpha || eval >= beta) && !self.stop {
                    // reduced window search failed
                    alpha = -MATE;
                    beta = MATE;
                    (eval, temp_best) = self.search_root(&board, alpha, beta, depth);
                }
            }

            if !self.stop {
                best_move = temp_best;
                if print_info { self.print_info(board.clone(), eval, depth); }
            }
            depth += 1;
        }

        (best_move, depth - 1)
    }
  
    // Separate function for searching the root. Saves temporary tt entries for root moves and
    // avoids a few optimizations. Allows returning the best move without pv retrieval.
    // Will be useful in case of future implementations of various
    // root-only heuristics
    fn search_root(
        &mut self,
        board: &Board,
        mut alpha: Eval,
        beta: Eval,
        mut depth: u8,
    ) -> (Eval, Move) {
        // Extend search depth when king is in check
        let in_check: bool = board.king_in_check(self.tables);
        if in_check { depth += 1; }

        // Probe tt only for best move
        match self.tt.probe(board.hash) {
            Some(entry) => self.sorter.tt_move = Some(entry.get_move()),
            None => self.sorter.tt_move = None
        };

        // Main recursive search block
        let mut moves_checked: u32 = 0;
        let mut eval: Eval;
        let mut best_move: Move = NULL_MOVE;
        let mut tt_entry = TTField::new(
            board.hash,
            TTFlag::Upper,
            NULL_MOVE,
            -MATE,
            depth,
            self.age,
            0
        );
        let moves: Vec<Move> = self.sorter.sort_moves(board.generate_moves(self.tables), 0);

        for m in moves {
            // only consider legal moves
            if let Some(new) = board.make_move(m, self.tables){
                moves_checked += 1;

                self.history.push(new.hash);
                if moves_checked == 1 {
                    // full search on first move
                    eval = -self.negamax(&new, -beta, -alpha, depth - 1, 1);
                    best_move = m;
                } else {
                    // only pvs in root node
                    eval = -self.negamax(&new, -alpha - 1, -alpha, depth - 1, 1);
                    if eval > alpha && eval < beta {
                        eval = -self.negamax(&new, -beta, -alpha, depth - 1, 1);
                    }
                };
                self.history.pop();

                if self.stop { return (0, NULL_MOVE); }

                if eval > alpha {               // possible pv node
                    best_move = m;
                    alpha = eval;

                    if eval >= beta {           // beta cutoff
                        if !(m.is_capture()) {
                            self.sorter.add_killer(m, 0);
                            self.sorter.add_history(m, depth);
                        };

                        tt_entry.update_data(TTFlag::Lower, best_move, beta);
                        self.tt.insert(tt_entry);
                        
                        return (beta, best_move);
                    }

                    // in root, we insert partial results for the other threads to use
                    tt_entry.update_data(TTFlag::Upper, best_move, alpha);
                    self.tt.insert(tt_entry);
                }
            }
        };

        if !self.stop { 
            // Insert value in tt
            tt_entry.update_data(TTFlag::Exact, best_move, alpha);
            self.tt.insert(tt_entry);
        }

        (alpha, best_move)
    }

    /// Fail-Hard Negamax
    fn negamax(
        &mut self,
        board: &Board,
        mut alpha: Eval,
        mut beta: Eval,
        mut depth: u8,
        ply: u8,
    ) -> Eval {
        // Check if search should be continued
        if self.stop || !self.clock.mid_check(self.nodes) {
            self.stop = true;
            return 0;
        }

        // Extend search depth when king is in check
        let in_check: bool = board.king_in_check(self.tables);
        let pv_node: bool = alpha != beta - 1; // False when in searching with a null window
        if in_check { depth += 1; }
        
        // Quiescence search to avoid horizon effect
        if depth == 0 { return self.quiescence(board, alpha, beta, ply); }

        // Mate distance pruning
        alpha = max(-MATE + ply as Eval, alpha);
        beta  = min( MATE - ply as Eval - 1, beta);
        if alpha >= beta { return alpha; }
        
        // Stop searching if the position is a rule-based draw (repetition/50mr)
        self.nodes += 1;
        if self.is_draw(board) { return 0; }
        
        // Probe tt for eval and best move
        match self.tt.probe(board.hash) {
            Some(entry) => {
                let tt_move = entry.get_move();

                if entry.get_depth() >= depth {
                    let tt_eval = entry.get_value(ply);
                    
                    match entry.get_flag() {
                        TTFlag::Exact => return tt_eval, 
                        TTFlag::Upper => beta = min(beta, tt_eval),
                        TTFlag::Lower => alpha = max(alpha, tt_eval),
                    }

                    // Upper/Lower flags can cause indirect cutoffs!
                    if alpha >= beta { return tt_eval; }
                }
                self.sorter.tt_move = Some(tt_move);
            }
            
            None => self.sorter.tt_move = None
        };

        // Apply null move pruning
        let mut eval: Eval;
        if depth > NMP_REDUCTION  && !pv_node && !in_check {
            let nmp: Board = board.make_null_move();
            
            eval = -self.negamax(&nmp, -beta, -beta + 1, depth - 1 - NMP_REDUCTION, ply + 1);
            if eval >= beta { return beta; }
        }

        // Main recursive search block
        let mut moves_checked: u32 = 0;
        let mut best_move: Move = NULL_MOVE;
        let mut tt_bound: TTFlag = TTFlag::Upper;
        let moves: Vec<Move> = self.sorter.sort_moves(board.generate_moves(self.tables), ply);

        for m in moves {
            // only consider legal moves
            if let Some(new) = board.make_move(m, self.tables) {
                moves_checked += 1;

                self.history.push(new.hash);
                if moves_checked == 1 {
                    // full depth search on first move
                    eval = -self.negamax(&new, -beta, -alpha, depth - 1, ply + 1);
                    best_move = m; // always init at least one best move
                } else {
                    // reduce depth for all moves beyond first
                    if  moves_checked >= LMR_THRESHOLD  &&
                        depth >= LMR_LOWER_LIMIT        &&
                        !in_check                       &&
                        !m.is_capture()                 &&
                        !m.is_promotion()
                    {
                        // LMR with a null window
                        eval = -self.negamax(&new, -alpha - 1, -alpha, depth - 2, ply + 1);
                    } else {
                        eval = alpha + 1; // else force pvs
                    }

                    if eval > alpha {
                        // normal PVS for any move beyond the first
                        eval = -self.negamax(&new, -alpha - 1, -alpha, depth - 1, ply + 1);
    
                        // sneaky way to also dodge re-searching when the window is already null
                        if eval > alpha && eval < beta {
                            // PVS failed
                            eval = -self.negamax(&new, -beta, -alpha, depth - 1, ply + 1);
                        }
                    }
                };
                self.history.pop();

                if self.stop { return 0; }

                if eval > alpha {               // possible pv node
                    if eval >= beta {           // beta cutoff
                        if !(m.is_capture()) {
                            self.sorter.add_killer(m, ply);
                            self.sorter.add_history(m, depth);
                        };
    
                        alpha = beta; // save correct value in tt
                        tt_bound = TTFlag::Lower;
                        break;
                    }

                    alpha = eval;
                    tt_bound = TTFlag::Exact;
                }

            }
        };
    
        // check for mate scores
        if moves_checked == 0 { // no legal moves
            if in_check {
                alpha = -MATE + ply as Eval;  // checkmate
            } else {
                alpha = 0; // stalemate
            }
        };

        if !self.stop { 
            // Insert value in tt
            let tt_entry = TTField::new(
                board.hash,
                tt_bound,
                best_move,
                alpha,
                depth,
                self.age,
                ply
            );
            self.tt.insert(tt_entry);
        }

        alpha
    }
    
    /// Quiescence search (only look at capture moves)
    fn quiescence(
        &mut self,
        board: &Board,
        mut alpha: Eval,
        beta: Eval,
        ply: u8,
    ) -> Eval {
        // Check if search should be continued
        if self.stop || !self.clock.mid_check(self.nodes) {
            self.stop = true;
            return 0;
        }

        self.nodes += 1;
        let mut eval: Eval = evaluate(board);   // try stand pat

        if ply >= MAX_DEPTH { return eval };
        
        if eval >= beta { return beta; };                   // beta cutoff
        if eval < alpha - FUTILITY_MARGIN { return alpha; } // futility pruning
        alpha = max(eval, alpha);                           // stand pat is pv
    
        let moves: Vec<Move> = self.sorter.sort_captures(board.generate_moves(self.tables));
        for m in moves {
            if let Some(new) = board.make_move(m, self.tables) {
                eval = -self.quiescence(&new, -beta, -alpha, ply + 1);
                
                if eval > alpha {                       // possible pv node
                    if eval >= beta { return beta; }    // beta cutoff
                    alpha = eval;
                }
            }
        }

        alpha // node fails low
    }

    /// Rule-based draw detection
    fn is_draw(&self, board: &Board) -> bool {
        board.halfmoves >= 100 || self.is_repetition(board)
    }

    /// Check for repetitions in hash history.
    /// We stop at the first occurrence of the position and consider that a draw.
    fn is_repetition(&self, board: &Board,) -> bool {
        self.history
            .iter()
            .rev()                       // step through history in reverse
            .take(board.halfmoves + 1)   // only check the last "halfmove" elements
            .skip(2)                     // first element is board itself, second is opponent. skip
            .step_by(2)                  // don't check opponent moves
            .any(|&x| { x == board.hash }) // stop at first occurrence
    }

    /// Recover pv from transposition table
    fn recover_pv(&self, mut board: Board, depth: u8) -> Vec<Move> {
        let mut pv: Vec<Move> = Vec::new();

        // traverse down the tree through the trasposition table
        for _ in 0..depth {
            let tt_move = match self.tt.probe(board.hash) {
                Some(e) => e.get_move(),
                None => break,
            };  

            // move "sanity" check, since a hash collision is possible
            let moves: Vec<Move> = board.generate_moves(self.tables).all_moves;
            if moves.contains(&tt_move) {
                match board.make_move(tt_move, self.tables) {
                    Some(b) => {
                        board = b;

                        pv.push(tt_move);
                    },

                    None => break,
                }
            } else {
                break;
            }
        }
        pv
    }

    /// Print UCI score info
    fn print_info(&self, board: Board, eval: Eval, depth: u8) {
        print!("info score ");

        if is_mate(eval) {          // mating
            print!("mate {} ", (MATE - eval + 1) / 2);
        } else if is_mated(eval) {  // mated
            print!("mate {} ", -(eval + MATE) / 2);
        } else {
            print!("cp {} ", eval);
        }

        print!("depth {} nodes {} pv ", depth, self.nodes / 1000);

        let pv = self.recover_pv(board, depth);
        for m in &pv { print!("{} ", m); }
        println!();
    }
}

/// Test nodes searched
/// Run with: cargo test --release search -- --show-output
#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::time::Instant;
    use std::sync::{
        Arc,
        atomic::AtomicBool
    };
    
    use crate::{
        clock::*,
        board::*,
        piece::Color,
    };

    fn search_driver(fen: &str, depth: u8) {
        let board: Board = Board::try_from(fen).unwrap();
        let tables: Tables = Tables::default();
        let tt: TT = TT::default();
        let clock: Clock = Clock::new(
            TimeControl::FixedDepth(depth),
            Arc::new(AtomicBool::new(false)),
            board.side == Color::White
        );
        let mut search: Search = Search::new(Vec::new(), clock, &tt, &tables);

        println!("\n{}\n\n", board);

        let start = Instant::now();
        let (best_move, _) = search.iterative_search(board, true);
        let duration = start.elapsed();

        println!("\nDEPTH: {} Found {} in {:?}\n--------------------------------\n", depth, best_move, duration);
    }

    #[test]
    fn search_kiwipete10() {
        search_driver(KIWIPETE_FEN, 10);
    }

    #[test]
    fn search_killer10() {
        search_driver(KILLER_FEN, 10);
    }

    #[test]
    fn search_mate4() {
        search_driver("8/8/8/2K5/5Q2/8/4k3/8 w - - 0 1", 20);
    }
}