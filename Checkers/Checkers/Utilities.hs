module Checkers.Utilities where

import Checkers.Types
import Data.List

-- Types
data Player = Red | Black
    deriving (Show, Eq)

-- Checking and Computation
-- Checks if peice is on 8x8 board
onboard :: Coord -> Bool
onboard (x,y) = (x < 8) && (0 <= x) && (y < 8) && (0 <= y)

-- Computes Euclidean distance between two coordinates
distance :: Coord -> Coord -> Int
distance (x1,y1) (x2,y2) = (abs (x1-x2)) + (abs (y1-y2))

unPorK :: PorK a -> a
unPorK (P a) = a
unPorK (K a) = a

addTup :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
addTup (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

isSimp :: Coord -> Coord -> Bool
isSimp c1 c2 = (distance c1 c2) == 2


-- GameState Modification Functions
deletePeice :: PorK Coord -> Player -> GameState -> GameState
deletePeice (P s) Red st = st{ redPieces = (delete s (redPieces st)) }
deletePeice (K s) Red st = st{ redKings = (delete s (redKings st)) }
deletePeice (P s) Black st = st{ blackPieces = (delete s (blackPieces st)) }
deletePeice (K s) Black st = st{ blackKings = (delete s (blackKings st)) }

addPeice :: PorK Coord -> Player -> GameState -> GameState
addPeice (P s) Red st = st{ redPieces = s:(redPieces st) }
addPeice (K s) Red st = st{ redKings = s:(redKings st) }
addPeice (P s) Black st = st{ blackPieces = s:(blackPieces st) }
addPeice (K s) Black st = st{ blackKings = s:(blackKings st) }