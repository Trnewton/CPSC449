module AI.ABTypes where

import Checkers.Types


data Value a
    -- | Bottom value
    = Bot
    -- | Values between bottom and top
    | Val a
    -- | Top value
    | Top
    deriving(Show, Eq, Ord)

data MinMax a
    -- | Tip of minmax tree with state evaluation
    =  TipMin a
    -- | Bracnh of minmax tree
    | BranchMin [MaxMin a]
    deriving (Show, Eq)

data MaxMin a
    -- | Tip of maxmin tree with state evaluation
    = TipMax a
    -- | Branch of maxmin tree
    | BranchMax [MinMax a]
    deriving (Show, Eq)

type Ply = Int
type StateEval = (Value Int)
