module Checkers.Moves where

import Checkers.Types
import qualified Data.Set as Set
import Checkers.Utilities


-- | Checks that move does not create a repeated state in checkers game
notRepeat :: Move -> GameState -> Bool
notRepeat m@[(K c1),(K c2)] st -- We only need to check simple king moves
    |isSimp c1 c2   = notRepeat' ([], [m]) (history st)
    |otherwise      = True
    where
        -- Recursively look further into history to look for possible repeated states
        notRepeat' :: ([Move], [Move]) -> [Move] -> Bool
        notRepeat' (ls, rs) (mv@[(K c1), (K c2)]:hist)
            |isSimp c1 c2 = case (ls', rs) of
                                ([], [])    -> False
                                _           -> notRepeat' (rs, ls') hist
            |otherwise = True
            where
                ls' = reduceCycles mv ls  -- Reduce paths in move list
        notRepeat' _ _ = True
notRepeat _ _ = True

{- | Performs a dfs to detect cycles in a move list after adding a new move and collapses
any connected edges. Assumes that no cycle existed before the new move.
-}
reduceCycles :: Move -> [Move] -> [Move]
reduceCycles [K start, K next] mvs = reduceCycles' start next [] mvs
    where
        reduceCycles' :: Coord -> Coord -> [Move] -> [Move] -> [Move]
        reduceCycles' start end seen [] = [K start, K end]:seen
        reduceCycles' start end seen (mv@[K start', K end']:ms)
            |(start==end') && (end==start') = seen ++ ms  -- Found cycle
            |(start==end') = reduceCycles' start' end seen ms  -- Add edge to start of path
            |(end==start') = reduceCycles' start end' seen ms  -- Add edge to end of path
            |otherwise = reduceCycles' start end (mv:seen) ms  -- Increase depth
reduceCycles _ mvs = mvs

-- | Computes next possible moves given a checkers game state.
moves :: GameState -> (SMorJM [Move])
moves st
    |jumpmoves /= []    = JM jumpmoves
    |simplemoves /= []  = SM simplemoves
    |otherwise          = EndM
    where
        jumpmoves = (jump_moves st)
        simplemoves = (simple_moves st)

-- | Uses a game state to compute the next possible jump movesin a checkers game.
jump_moves :: GameState -> [Move]
jump_moves st
    |status st == RedPlayer     = (jumpPawn (redPieces st) (-1)) ++ (jumpKing (redKings st))
    |status st == BlackPlayer   = (jumpPawn (blackPieces st) 1) ++ (jumpKing (blackKings st))
    |otherwise                  = []
    where
        -- | Compute jump pawn moves
        jumpPawn :: PieceState -> Int -> [Move]
        jumpPawn coords dir = [(P (x,y)):moves | (x,y) <- coords, moves <- jumpPawn' (x,y) [] (x,y)]
            where
                -- Compute possible jump moves
                jumpPawn' start rem (x,y) =
                    [(t (x'', y'')):moves
                    | (dx, dy) <- [(-1, 0 + dir),(1, 0 + dir)]  -- Direction of move
                    , let (x',y') = addTup (x,y) (dx,dy)  -- Capture coordinate
                    , Set.member (x', y') opponentOccupied
                    , let (x'',y'') = addTup (x',y') (dx,dy)  -- Coordinate after jump
                    , onboard (x'', y'')
                    , (Set.notMember (x'', y'') occupied) || (start == (x'',y''))
                    , not (elem (x',y') rem)
                    , let isOnBoard = onboard (x'', y'' + dir)
                    , let t = if isOnBoard then P else K  -- Checks if pawn has been promoted
                    , moves <- (if isOnBoard
                                then jump_over (jumpPawn' start ((x',y'):rem) (x'',y''))
                                else jump_over (jumpKing' start ((x',y'):rem) (x'',y'')))]

        -- | Compute jump king moves
        jumpKing :: PieceState  -> [Move]
        jumpKing coords = [(K (x,y)):moves | (x,y) <- coords, moves <- jumpKing' (x,y) [] (x,y)]
        jumpKing' start rem (x,y) =
            [(K (x'', y'')):moves
            | (dx, dy) <- [(-1,-1), (-1,1), (1,-1), (1,1)]  -- Direction of move
            , let (x',y') = addTup (x,y) (dx,dy)  -- Capture coordinate
            , Set.member (x', y') opponentOccupied
            , let (x'',y'') = addTup (x',y') (dx,dy)  -- Coordinate after jump
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

-- | Uses a game state to compute the next possible simple moves in a checkers game.
simple_moves :: GameState -> [Move]
simple_moves st
    -- How we create moves depends on if its red or black to move
    |status st == RedPlayer     = (simplePawn (redPieces st) (-1)) ++ (simpleKing (redKings st))
    |status st == BlackPlayer   = (simplePawn (blackPieces st) 1) ++ (simpleKing (blackKings st))
    |otherwise                  = []
    where
        -- List of possible simple pawn moves
        simplePawn coords dir =
            [[P (x, y), t (x', y')]
            | (x, y) <- coords  -- Start coord of possible moves
            , (x', y') <- let y' = y + dir in [(x-1,y'),(x+1,y')]  -- End coord of possible moves
            , onboard (x', y')
            , Set.notMember (x', y') occupied
            , let t = if onboard (x', y' + dir) then P else K]  -- See if peice is pawn or king

        -- List of possible simple king moves
        simpleKing coords =
            [[K (x, y), K (x', y')]
            | (x, y) <- coords  -- Start coord of possible moves
            , (x', y') <- [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1)]  -- End coord of possible moves
            , onboard (x', y')
            , Set.notMember (x', y') occupied
            , notRepeat [(K (x, y)), (K (x', y'))] st]  -- Check for repeated state

        -- Set of occupied coords
        occupied = Set.fromList ((redPieces st) ++ (redKings st) ++ (blackPieces st) ++ (blackKings st))
