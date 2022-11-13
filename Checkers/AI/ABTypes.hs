module AI.ABTypes where

import Checkers.Types


data Value a = Bot | Val a | Top
    deriving(Show, Eq, Ord)

data MinMax a = BranchMin a [MaxMin a]
    deriving (Show, Eq)

data MaxMin a = BranchMax a [MinMax a]
    deriving (Show, Eq)

type Ply = Int

data StateEval =
        StateEval   { gamestate :: GameState
                    , eval :: (Value Int) }
        deriving (Show)

instance Eq StateEval where
    stEv1 == stEv2 = (eval stEv1) == (eval stEv2)

instance Ord StateEval where
    compare stEv1 stEv2 = compare (eval stEv1) (eval stEv2)
