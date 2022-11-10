module Checkers.ApplyMove where

import Checkers.Moves
import Checkers.Types
import Checkers.Utilities

import Data.List

------------------------------------------------
------------------------------------------------
--
--  APPLYING A MOVE to a state
--
------------------------------------------------
------------------------------------------------

-- | Updates checkers game state in accordance with a given move,
apply_move:: Move -> GameState -> GameState
apply_move mv st
    |posMoves == EndM = st{ message = "Game Over.", status = GameOver }
    |isMove mv posMoves =
        (case posMoves of  -- Determine if we have a simple or jump move
            SM _ -> make_simple_move mv st
            JM _ -> make_jump_move mv st)
            { history = mv:(history st), message = "" }
    |otherwise = st{ message=("Bad move.") }
    where
        -- Possible legal moves
        posMoves = moves st

        -- Checks if move is possible
        isMove :: Move -> SMorJM [Move] -> Bool
        isMove mv (SM mvs) = elem mv mvs
        isMove mv (JM mvs) = elem mv mvs
        isMove _ _ = False  -- if EndM

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