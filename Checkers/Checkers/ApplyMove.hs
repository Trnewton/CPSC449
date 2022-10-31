module Checkers.ApplyMove where

import Checkers.Moves
import Checkers.Types

import Data.List

------------------------------------------------
------------------------------------------------
--
--  APPLYING A MOVE to a state
--
------------------------------------------------
------------------------------------------------

apply_move:: Move -> GameState -> GameState
apply_move mv st
    | elem mv posMoves = apply_move' mv st  { history = mv:(history st)
                                            , message = ""}
    | otherwise = st{message=("Bad move.")}
    where
        posMoves = case moves st of SM mvs -> mvs ; JM mvs -> mvs ; EndM -> []
        -- Determine if we have a simple or jump move
        apply_move' mv@[p1, p2] st
            | (abs (x1-x2)) + (abs (y1-y2)) == 2    = make_simple_move mv st
            | otherwise                             = make_jump_move mv st
            where
                (x1,y1) = case p1 of (P (x,y)) -> (x,y) ; (K (x,y)) -> (x,y)
                (x2,y2) = case p2 of (P (x,y)) -> (x,y) ; (K (x,y)) -> (x,y)
        apply_move' mv st = make_jump_move mv st

        -- Update state for simple move
        make_simple_move [p1, p2] st
            | status st == BlackPlayer = -- We are movning a black piece
                (let st' = case p1 of -- Remove first coord of move
                            (P s) -> st{blackPieces = (delete s (blackPieces st)) }
                            (K s) -> st{blackKings = (delete s (blackKings st)) }
                in case p2 of -- Add coordinate of last move
                    (P s) -> st'{blackPieces = s:(blackPieces st')}
                    (K s) -> st'{blackKings = s:(blackKings st')}){
                        status = RedPlayer
                    }
            | otherwise = -- We are movning a red piece
                (let st' = case p1 of -- Remove first coord of move
                            (P s) -> st{redPieces = (delete s (redPieces st)) }
                            (K s) -> st{redKings = (delete s (redKings st)) }
                in case p2 of -- Add coordinate of last move
                    (P s) -> st'{redPieces = s:(redPieces st')}
                    (K s) -> st'{redKings = s:(redKings st')}){
                        status = BlackPlayer
                    }

        -- Update state for jump move
        make_jump_move mvs@(p:_) st
            | status st == BlackPlayer = -- We are movning a black piece
                (let st' = case p of -- Remove first coord of move and captures
                            (P s) -> st{blackPieces = (delete s (blackPieces st))
                                    , redPieces = (redPieces st \\ rem)
                                    , redKings = (redKings st \\ rem)}
                            (K s) -> st{blackKings = (delete s (blackKings st))
                                    , redPieces = (redPieces st \\ rem)
                                    , redKings = (redKings st \\ rem)}
                in case p2 of -- Add coordinate of last move
                    (P s) -> st'{blackPieces = s:(blackPieces st')}
                    (K s) -> st'{blackKings = s:(blackKings st')}){
                        status = RedPlayer
                    }
            | otherwise = -- We are movning a red piece
                (let st' = case p of -- Remove first coord of move and captures
                            (P s) -> st{redPieces = (delete s (redPieces st))
                                , blackPieces = (blackPieces st \\ rem)
                                , blackKings = (blackKings st \\ rem)}
                            (K s) -> st{redKings = (delete s (redKings st))
                                , blackPieces = (blackPieces st \\ rem)
                                , blackKings = (blackKings st \\ rem)}
                in case p2 of -- Add coordinate of last move
                    (P s) -> st'{redPieces = s:(redPieces st')}
                    (K s) -> st'{redKings = s:(redKings st')}){
                        status = BlackPlayer
                    }
            where
                -- Compute captues and get last coord of moving piece
                (rem, p2) = make_jump_move' mvs st
                -- make_jump_move' :: Moves -> GameSTate -> Moves ->
                make_jump_move' (coord:[]) _ = ([], coord)
                make_jump_move' (c1:c2:cs) st = ((cap:caps), end)
                    where
                        (caps, end) = make_jump_move' (c2:cs) st
                        cap = (quot (x1 + x2) 2, quot (y1 + y2) 2)
                        (x1,y1) = case c1 of (P (x,y)) -> (x,y) ; (K (x,y)) -> (x,y)
                        (x2,y2) = case c2 of (P (x,y)) -> (x,y) ; (K (x,y)) -> (x,y)