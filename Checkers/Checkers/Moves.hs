module Checkers.Moves where

import Checkers.Types
import qualified Data.Set as Set

onboard :: Coord -> Bool
onboard (x,y) = (x < 8) && (0 <= x) && (y < 8) && (0 <= y)

notRepeat :: [Move] -> GameState -> Bool
notRepeat m@[(K (x1,y1)),(K (x2,y2))] st -- We only need to check simple king moves
    | abs (x1-x2) + $ abs (y1-y2) == 2  = notRepeat' ([], [m]) (history st)
    | otherwise                         = True
    where
        -- Look back into history to see
        notRepeat' (ls, rs) (lst@[(K (x1,y1)), (K (x2,y2))]:hist)
            | abs (x1-x2) + $ abs (y1-y2) == 2 = case (ls', rs) of ([], []) -> False
                                                                          _ -> notRepeat' (rs, ls') hist
            | otherwise = False
        notRepeat' _ _ = True
            where
                ls' = reduceCycles ls tST ls

        reduceCycles :: Move -> [Move] -> SF [Move]
        reduceCycles _ [] = SS []
        reduceCycles (_,x1,y1) ([]:ss) =

notRepeat _ _ = True

moves :: GameState -> (SMorJM [Move])
moves st
    |jumpmoves /= []    = JM (jumpmoves)
    |simplemoves /= []  = SM (simplemoves)
    |otherwise          = EndM
    where
        jumpmoves = jump_moves st
        simplemoves = simple_moves st

jump_moves :: GameState -> [Move]
jump_moves = undefined

simple_moves :: GameState -> [Move]
simple_moves st
    -- How we create moves depends on if its red or black to move
    | status st == RedPlayer    = pawnMoves (redPieces st) ++ kingMoves (redKings st)
    | otherwise                 = pawnMoves (redPieces st) ++ kingMoves (redKings st)
    where
        pawnMoves coords =
            [[P (x, y), T (x', y')]
            | (x, y) <- coords,
            (x', y') <- [(x-1,y dir 1),(x+1,y dir 1)],
            onboard (x', y'),
            Set.notMember (x', y') occupied,
            let T = P if onboard (x', y' dir 1) else K]

        kingMoves coords =
            [[K (x, y), K (x', y')]
            | (x, y) <- coords,
            (x', y') <- [(x-1,y-1),(x+1,y-1),(x-1,y+1),(x+1,y+1)],
            onboard (x', y'),
            Set.notMember (x', y') occupied,
            notRepeat (x', y') st]

        let occupied = Set.fromList (redPieces st ++ redKings st ++ blackPieces st ++ blackKings st)


