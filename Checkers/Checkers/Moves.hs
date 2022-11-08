module Checkers.Moves where

import Checkers.Types
import qualified Data.Set as Set
import Checkers.Utilities

data SF a = SS a | FF



-- TODO: Clean this up, might also be able to make more efficient by doing cycle
-- detection all in one instead of at each past state
notRepeat :: Move -> GameState -> Bool
notRepeat m@[(K c1),(K c2)] st -- We only need to check simple king moves
    |isSimp c1 c2   = notRepeat' ([], [m]) (history st)
    |otherwise      = True
    where
        -- Look back into history to see
        notRepeat' :: ([Move], [Move]) -> [Move] -> Bool
        notRepeat' (ls, rs) (mv@[(K c1), (K c2)]:hist)
            |isSimp c1 c2 = case (ls', rs) of
                                ([], []) -> False
                                _ -> notRepeat' (rs, ls') hist
            |otherwise = True
            where
                ls' = reduceCycles mv ls
        notRepeat' _ _ = True
notRepeat _ _ = True

reduceCycles :: Move -> [Move] -> [Move]
reduceCycles mv@[K start, K nxt] ms =
    case reduceCycles' start nxt [] ms of   SS ms' -> ms'
                                            FF -> (mv:ms)
    where
        -- TODO: Make this into a foldr
        -- TODO: This is not very efficient
        reduceCycles' :: Coord -> Coord -> [Move] -> [Move] -> SF [Move]
        reduceCycles' _ _ _ [] = FF
        reduceCycles' start coord seen (mv@[K coord1, K coord2]:ms)
            -- We have a new edge in our dfs
            |coord==coord1 = if (start==coord2)
                                -- Cycle located
                                then SS (seen ++ ms)
                                -- Look deeper
                                else reduceCycles' start coord2 [] (seen ++ ms)
            -- Move to next possble edge
            |otherwise = reduceCycles' start coord (mv:seen) ms


moves :: GameState -> (SMorJM [Move])
moves st
    |jumpmoves /= []    = JM jumpmoves
    |simplemoves /= []  = SM simplemoves
    |otherwise          = EndM
    where
        jumpmoves = (jump_moves st)
        simplemoves = (simple_moves st)

jump_moves :: GameState -> [Move]
jump_moves st
    |status st == RedPlayer     = (jumpPawn (redPieces st) (-1)) ++ (jumpKing (redKings st))
    |status st == BlackPlayer   = (jumpPawn (blackPieces st) 1) ++ (jumpKing (blackKings st))
    |otherwise                  = []
    where
        -- Compute jump pawn moves
        jumpPawn :: PieceState -> Int -> [Move]
        jumpPawn coords dir = [(P (x,y)):moves | (x,y) <- coords, moves <- jumpPawn' (x,y) [] (x,y)]
            where
                jumpPawn' start rem (x,y) =
                    [(t (x'', y'')):moves
                    | (dx, dy) <- [(-1, 0 + dir),(1, 0 + dir)]
                    , let (x',y') = addTup (x,y) (dx,dy)
                    , Set.member (x', y') opponentOccupied
                    , let (x'',y'') = addTup (x',y') (dx,dy)
                    , onboard (x'', y'')
                    , (Set.notMember (x'', y'') occupied) || (start == (x'',y''))
                    , not (elem (x',y') rem)
                    , let isOnBoard = onboard (x'', y'' + dir)
                    , let t = if isOnBoard then P else K
                    , moves <- (if isOnBoard
                                then jump_over (jumpPawn' start ((x',y'):rem) (x'',y''))
                                else jump_over (jumpKing' start ((x',y'):rem) (x'',y'')))]

        -- Compute jump king moves
        jumpKing :: PieceState  -> [Move]
        jumpKing coords = [(K (x,y)):moves | (x,y) <- coords, moves <- jumpKing' (x,y) [] (x,y)]
        jumpKing' start rem (x,y) =
            [(K (x'', y'')):moves
            | (dx, dy) <- [(-1,-1), (-1,1), (1,-1), (1,1)]
            , let (x',y') = addTup (x,y) (dx,dy)
            , Set.member (x', y') opponentOccupied
            , let (x'',y'') = addTup (x',y') (dx,dy)
            , onboard (x'', y'')
            , (Set.notMember (x'', y'') occupied) || (start == (x'',y''))
            , not (elem (x',y') rem)
            , moves <- jump_over (jumpKing' start ((x',y'):rem) (x'',y''))]

        -- Wrapper thing
        jump_over [] = [[]]
        jump_over z = z

        -- Sets of coords to look for
        opponentOccupied = Set.fromList (if status st == RedPlayer
                                            then ((blackPieces st) ++ (blackKings st))
                                            else ((redPieces st) ++ (redKings st)))
        occupied = Set.fromList ((redPieces st) ++ (redKings st) ++ (blackPieces st) ++ (blackKings st))


simple_moves :: GameState -> [Move]
simple_moves st
    -- How we create moves depends on if its red or black to move
    |status st == RedPlayer     = (simplePawn (redPieces st) (-1)) ++ (simpleKing (redKings st))
    |status st == BlackPlayer   = (simplePawn (blackPieces st) 1) ++ (simpleKing (blackKings st))
    |otherwise                  = []
    where
        simplePawn coords dir =
            [[P (x, y), t (x', y')]
            | (x, y) <- coords -- Start coord of possible moves
            , (x', y') <- let y' = y + dir in [(x-1,y'),(x+1,y')] -- End coord of possible moves
            , onboard (x', y')
            , Set.notMember (x', y') occupied
            , let t = if onboard (x', y' + dir) then P else K] -- See if peice is pawn or king

        simpleKing coords =
            [[K (x, y), K (x', y')]
            | (x, y) <- coords -- Start coord of possible moves
            , (x', y') <- [(x+1,y+1),(x+1,y-1),(x-1,y+1),(x-1,y-1)] -- End coord of possible moves
            , onboard (x', y')
            , Set.notMember (x', y') occupied
            , notRepeat [(K (x, y)), (K (x', y'))] st] -- Check for repeated state

        -- Set of occupied coords
        occupied = Set.fromList ((redPieces st) ++ (redKings st) ++ (blackPieces st) ++ (blackKings st))
