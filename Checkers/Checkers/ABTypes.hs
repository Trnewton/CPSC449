module AI.ABTypes where


data Value a = Bot | Val a | Top
    deriving(Show, Eq, Ord)

data MinMaxTree a
    = Leaf a
    | Node (MinMaxTree a) (MinMaxTree a)
    deriving (Show)

-- foldTree :: MinMaxTree a