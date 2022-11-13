module AI.ABsearch where

import Checkers.Types
import Checkers.Moves
import Checkers.Utilities

import AI.ABTypes


-- Heuristics

black_heuristic :: GameState -> Value Int
black_heuristic st = undefined

red_heuristic :: GameState -> Value Int
red_heuristic st = undefined


-- Search

samePlayer :: Player -> GameState -> Bool
samePlayer Red st = (status GameState) == RedPlayer
samePlayer Black st = (status GameState) == BlackPlayer

{-
TODO:
1) Function to create search tree
    a) Should not evaluate the whole tree otherwise pruning is useless
    b) Might want to do something special with jump moves
-}
makeTree :: Ply ->  Player -> (GameState -> (Value Int)) -> GameState -> MaxMin StateEval
makeTree 0 py eval st
    |samePlayer py st   = (BranchMax (eval st) [])
    |otherwise          = (BranchMin (eval st) [])
makeTree n py eval st = case (moves st) of
    -- SM mvs -> Node map (\mv -> makeTree n-1 eval (apply_move mv st)) mvs
    -- JM mvs -> Node map (\mv -> makeTree n-1 eval (apply_move mv st)) mvs
    EndM -> if samePlayer py st then (BranchMax Bot []) else (BranchMin Top st [])


maxeval :: MaxMin StateEval -> StateEval
maxeval (BranchMax stEv ms) = StateEval { gamestate = (gamestate stEv)
                                        , eval = (foldl max Bot (map (eval.mineval) ms))}

mineval :: MinMax StateEval -> StateEval
mineval (BranchMin stEv ms) = StateEval { gamestate = (gamestate stEv)
                                        , eval = (foldl min Top (map (eval.maxeval) ms))}

minMaxSearch :: GameState -> Move
minMaxSearch st = undefined


abSearch :: MinMax Int -> Move
abSearch tr = undefined
