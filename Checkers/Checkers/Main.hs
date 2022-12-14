module Main where

import Checkers.Types
--------------------------------------------------------------------------------------
--                     Imports you will need to develop
--------------------------------------------------------------------------------------
import Checkers.Moves
import Checkers.ApplyMove
import Checkers.Tests
import AI.ABsearch
--------------------------------------------------------------------------------------
--                     Standard library imports:
--------------------------------------------------------------------------------------
-- 'System.IO' is needed to set the line buffering (i.e., so we can press backspace
--  in GHCi to do what we expect it should do)
import System.IO
-- 'Text.Read' is needed for 'readMaybe' since 'read' throws an exception by default
--  when it parses, but we want to handle the exception ourselves with the 'Maybe' type.
import Text.Read
import Data.Time
--------------------------------------------------------------------------------------

-- main :: IO ()
main =
    -- do print (show testabsearch)
    do -- the Human on Human game configuration
        hSetBuffering stdin LineBuffering
        runGame $ GameConfig { movemaker = apply_move
                            , blackMove = AI (abSearch)
                            , redMove = AI (abSearch)
                            , state = initialGameState}


{-   Example of configurations for games:

main = do hSetBuffering stdin LineBuffering
          runGame $ GameConfig {movemaker = apply_move
                             , blackMove = AI (ai_move_black)
                             , redMove = AI (ai_move_red)
                             , state = initialGameState}


main = do hSetBuffering stdin LineBuffering
          runGame $ GameConfig {movemaker = apply_move
                             , blackMove = Human
                             , redMove = AI (ai_move_red)
                             , state = initialGameState}


main = do hSetBuffering stdin LineBuffering
          runGame $ GameConfig {movemaker = apply_move
                             , blackMove = AI (ai_move_black)
                             , redMove = Human
                             , state = initialGameState}

-}

--  Running a game ...

runGame :: GameConfig -> IO ()
runGame g = case (currentMove g) of
    Nothing -> do  -- gameover
        print (state g)
    Just Human -> do  -- Human player
        print (state g)
        print "Enter move as a list of coordinates."
        -- The loop to keep asking for input until the user inputs a valid move.
        let loop = fmap readMaybe getLine >>= \ma -> case ma of
                   Just mv -> return mv
                   Nothing -> putStrLn "Please try again..." >> loop
        move <- loop

        let s' = (movemaker g) move (state g)
        runGame $ g{state = s'}
    Just (AI ai_move) -> do -- AI player
        print (state g)
        time <- getCurrentTime
        print time
        runGame $ g{state = (movemaker g) (ai_move (state g)) (state g)}
  where
    currentMove :: GameConfig -> Maybe PlayerType
    currentMove conf = case status (state conf) of
      RedPlayer -> Just (redMove conf)
      BlackPlayer -> Just (blackMove conf)
      GameOver -> Nothing




