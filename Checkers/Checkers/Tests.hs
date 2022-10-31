module Checkers.Tests where

import Checkers.Types
import Checkers.Moves

aiPrePlanned :: [Move] -> GameState -> Move
aiPrePlanned mvs st = mvs !! (length (history st))

--Some test states ...
-- Basic
test1 = GameState { blackPieces = []
                  , redPieces = []
                  , blackKings = [(0,1)]
                  , redKings = [(0,3),(2,3)]
                  , status = RedPlayer
                  , message = ""
                  , history = []}

test2 = GameState { blackPieces = []
                  , redPieces = []
                  , blackKings = [(6,3),(4,3)]
                  , redKings = [(0,1)]
                  , status = RedPlayer
                  , message = ""
                  , history = []}

test3 =  GameState { blackPieces = []
                    , redPieces = [(6,5)]
                    , blackKings = [(6,3),(4,3)]
                    , redKings = [(0,1),(0,5)]
                    , status = RedPlayer
                    , message = ""
                    , history = []}


-- Repeated State Tests

rep1 = GameState {
    blackPieces = [],
    redPieces = [],
    blackKings = [(6,7)],
    redKings = [(2,1),(3,2)],
    status = RedPlayer,
    message = "",
    history = [
        [K (5,6),K (6,7)],
        [K (1,2),K (2,1)],
        [K (6,5),K (5,6)],
        [K (2,3),K (1,2)],
        [K (7,6),K (6,5)],
        [K (2,1),K (3,2)],
        [K (6,7),K (7,6)]
    ]
}

rep2 = GameState {
    blackPieces = [],
    redPieces = [],
    blackKings = [(6,7)],
    redKings = [(3,2),(2,3)],
    status = RedPlayer,
    message = "",
    history = [
        [K (5,6),K (6,7)],-- B
        [K (1,2),K (2,1)],-- R
        [K (6,5),K (5,6)],-- B
        [K (2,3),K (1,2)],-- R
        [K (7,6),K (6,5)],-- B
        [K (2,1),K (3,2)],-- R
        [K (6,7),K (7,6)] -- B
    ]
}