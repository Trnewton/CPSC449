module Checkers.Moves where

import Checkers.Types
import qualified Data.Set as Set

onboard :: Coord -> Bool
onboard (x,y) = (x < 8) && (0 <= x) && (y < 8) && (0 <= y)

-- TODO: Clean this up, might also be able to make more efficient by doing cycle
-- detection all in one instead of at each past state
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
                ls' = reduceCycles lst ls

        -- TODO: This requires alot of cleaning
        reduceCycles' :: Move -> [Move] -> [Move]
        reduceCycles _ _ [] = []
        reduceCycles mv@[_ start, _ nxt] ms =
            case reduceCycles' start nxt ms of  SS ms' -> ms'
                                                FF -> mv:ms
            where
                -- TODO: Make this into a foldr
                -- We already know that all the moves at this point are simple
                -- king moves. As we go deeper into the move trace we can add moves
                -- to the possible cycle if they start at the current end of the
                -- possible cycle. We then know we've found a cycle with a SS
                reduceCycles' :: Coord -> Coord -> [Move] -> SF [Move]
                reduceCycles' _ _ [] = FF []
                reduceCycles' (xs,ys) (x,y) (mv@[(_ (x1, y1), (_ (x2, y2)))]:ms)
                    | (x==x1) && (y==y1) =
                        if ((xs==x2)&&(ys=y2)) then SS ms else
                            (case reduceCycles' (xs,ys) (x,y) ms of SS ms' -> ms'
                                                                    FF -> FF)
                    | otherwise = case reduceCycles' (xs,ys) (x,y) ms of    SS ms' -> mv:ms'
                                                                            FF -> FF
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


