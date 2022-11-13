module AI.ABsearch where

import Checkers.Types
import Checkers.Moves
import Checkers.Utilities
import Checkers.ApplyMove

import AI.ABTypes

import Data.List
import Debug.Trace

-- Constants
maxPly = 6
kingWeight = 10
pawnWeight = 1

-- Heuristics

-- | Heuristic for evaluating checkers game state from perspective of black player.
black_heuristic :: GameState -> Value Int
black_heuristic st = Val (kingWeight * kingFactor + pawnWeight * pieceFactor)
    where
        kingFactor = length (blackKings st) - (length (redKings st))
        pieceFactor = length (blackPieces st) - (length (redPieces st))

-- | Heuristic for evaluating checkers game state from perspective of red player.
red_heuristic :: GameState -> Value Int
red_heuristic st = Val (kingWeight * kingFactor + pawnWeight * pieceFactor)
    where
        kingFactor = length (redKings st) - (length (blackKings st))
        pieceFactor = length (redPieces st) - (length (blackPieces st))


-- Search
-- | Creates a game min max tree starting from the perspective of the maximizing player
makeMaxTree :: Ply -> (GameState -> StateEval) -> GameState -> MaxMin StateEval
makeMaxTree 0 eval st = case (moves st) of  -- Reached desired ply but apply "waiting for quiescence"
                SM mvs -> TipMax (eval st)
                JM mvs -> BranchMax (map (\mv -> makeMinTree 0 eval (apply_move_no_check mv st)) mvs)
                EndM -> TipMax Bot
makeMaxTree n eval st = case (moves st) of  -- Determine what type of moves we have and treat accordingly
                SM mvs -> BranchMax (map (\mv -> makeMinTree (n-1) eval (apply_move_no_check mv st)) mvs)
                JM mvs -> BranchMax (map (\mv -> makeMinTree n eval (apply_move_no_check mv st)) mvs)
                EndM -> TipMax Bot

-- | Creates a game min max tree starting from the perspective of the minimizing player
makeMinTree :: Ply -> (GameState -> StateEval) -> GameState -> MinMax StateEval
makeMinTree 0 eval st = case (moves st) of
                SM mvs -> TipMin (eval st)
                JM mvs -> BranchMin (map (\mv -> makeMaxTree 0 eval (apply_move_no_check mv st)) mvs)
                EndM -> TipMin Top
makeMinTree n eval st = case (moves st) of
                SM mvs -> BranchMin (map (\mv -> makeMaxTree (n-1) eval (apply_move_no_check mv st)) mvs)
                JM mvs -> BranchMin (map (\mv -> makeMaxTree n eval (apply_move_no_check mv st)) mvs)
                EndM -> TipMin Top


-- Basic Min Max Search
-- | Performs a min max evaluation of a minmax tree starting with the max player
maxeval :: MaxMin StateEval -> StateEval
maxeval (TipMax v) = v
maxeval (BranchMax branches) = (foldr max Bot (map mineval branches))

-- | Performs a min max evaluation of a minmax tree starting with the min player
mineval :: MinMax StateEval -> StateEval
mineval (TipMin v) = v
mineval (BranchMin branches) = (foldr min Top (map maxeval branches))

-- | Permform min max search starting with a gamestate and returning a maximizing move for current player
minMaxSearch :: GameState -> Move
minMaxSearch st = snd $argmax fst (map (\mv -> (moveEval mv, mv)) mvs)  -- Take best move
    where
        -- Evaluate moves while keeping move
        moveEval mv = mineval (makeMinTree maxPly heuristic (apply_move_no_check mv st))
        heuristic = case (status st) of
                        RedPlayer -> red_heuristic
                        BlackPlayer -> black_heuristic
        -- Possible moves
        mvs = case (moves st) of
            SM mvs -> mvs
            JM mvs -> mvs
            EndM -> []

{- | Permform min max search with alpha-beta pruning starting with a gamestate and
returning a maximizing move for current player
-}
abSearch :: GameState -> Move
abSearch st = snd $argmax fst (map (\mv -> (moveEval mv, mv)) mvs)
    where
        moveEval mv = abminprune (makeMinTree maxPly heuristic (apply_move_no_check mv st)) Bot Top
        heuristic = case (status st) of
                        RedPlayer -> red_heuristic
                        BlackPlayer -> black_heuristic

        mvs = case (moves st) of
            SM mvs -> mvs
            JM mvs -> mvs
            EndM -> []

-- NOTE: Alpha-Beta pruning code was adapted from CPSC 449 Course Notes
-- | Evaluates min max tree with alpha-beta pruning from maximizing player
abmaxprune :: (MaxMin StateEval) -> StateEval -> StateEval -> StateEval
abmaxprune t a b| a==b = a -- cutoff
                | otherwise = case t of
                      (TipMax v) -> min (max v a) b  -- Evaluate tip
                      (BranchMax ts) -> abmaxprunes ts a b

-- | Updates alpha and switches to the minimizing player in alpha-beta pruning
abmaxprunes :: [MinMax StateEval] -> StateEval -> StateEval -> StateEval
abmaxprunes [] a b = a
abmaxprunes (t:ts) a b = abmaxprunes ts newa b  -- Update alpha from last branch search and go to next
    where
        newa = abminprune t a b  -- Perform minimizing search

-- | Evaluates min max tree with alpha-beta pruning from minimizing player
abminprune ::(MinMax StateEval) -> StateEval -> StateEval -> StateEval
abminprune t a b| a==b = b --cutoff
                 | otherwise = case t of
                      (TipMin v) -> max (min v b) a  -- Evaluate tip
                      (BranchMin ts) -> abminprunes ts a b

-- | Updates beta and switches to the maximizing player in alpha-beta pruning
abminprunes :: [MaxMin StateEval] -> StateEval -> StateEval -> StateEval
abminprunes [] a b = b
abminprunes (t:ts) a b = abminprunes ts a newb  -- Update beta from last branch search and go to next
    where
        newb = abmaxprune t a b  -- Perform maximizing search


-- Helper functions

-- | Updates checkers game state in accordance with a given move, but assumes that move is valid.
apply_move_no_check:: Move -> GameState -> GameState
apply_move_no_check mv@[c1,c2] st
    |isSimp (unPorK c1) (unPorK c2) = (make_simple_move mv st){ history = mv:(history st), message = "" }
    |otherwise = (make_jump_move mv st){ history = mv:(history st), message = "" }
apply_move_no_check mv st = (make_jump_move mv st){ history = mv:(history st), message = "" }

-- Update state for simple move
make_simple_move :: Move -> GameState -> GameState
make_simple_move [p1, p2] st
    |status st == BlackPlayer =  -- We are moving a black piece
        (addPeice p2 Black (deletePeice p1 Black st)){ status = RedPlayer }
    |otherwise =  -- We are moving a red piece
        (addPeice p2 Red (deletePeice p1 Red st)){ status = BlackPlayer }

-- Update state for jump move
make_jump_move :: Move -> GameState -> GameState
make_jump_move mvs@(p1:_) st
    |status st == BlackPlayer =  -- We are moving a black piece
        (addPeice p2 Black (deletePeice p1 Black st)){
                status = RedPlayer,
                -- Remove captured peices
                redPieces = (redPieces st \\ rem),
                redKings = (redKings st \\ rem)
            }
    |otherwise = -- We are moving a red piece
        (addPeice p2 Red (deletePeice p1 Red st)){
                status = BlackPlayer,
                -- Remove captured peices
                blackPieces = (blackPieces st \\ rem),
                blackKings = (blackKings st \\ rem)
            }
    where
        -- Compute captues and get last coord of moving piece
        (rem, p2) = make_jump_move' mvs st

        -- Computes list of peices to remove and final position for jump move
        make_jump_move' :: Move -> GameState -> ([Coord], PorK Coord)
        make_jump_move' (coord:[]) _ = ([], coord)
        make_jump_move' (c1:c2:cs) st = ((cap:caps), end)
            where
                (caps, end) = make_jump_move' (c2:cs) st
                -- Captured peice
                cap = (quot (x1 + x2) 2, quot (y1 + y2) 2)
                (x1,y1) = unPorK c1
                (x2,y2) = unPorK c2