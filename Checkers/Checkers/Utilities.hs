module Checkers.Utilities where

import Checkers.Types


-- Checking and Computation
onboard :: Coord -> Bool
onboard (x,y) = (x < 8) && (0 <= x) && (y < 8) && (0 <= y)

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
