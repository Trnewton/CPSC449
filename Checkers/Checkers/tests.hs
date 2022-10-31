import Checkers.Types

aiPrePlanned :: [Moves] -> GameState

--Some test states ...

test1 = GameState { blackPieces = []
                  , redPieces = []
                  , blackKings = [(0,1)]
                  , redKings = [(0,3),(2,3)]
                  , status = RedPlayer
                  , message = ""
                  , history = []}

test2 = GameState { blackPieces = []
                  , redPieces = []
                  , blackKings = [(6,3),(4,3)]
                  , redKings = [(0,1)]
                  , status = RedPlayer
                  , message = ""
                  , history = []}

test3 =  GameState { blackPieces = []
                  , redPieces = [(6,5)]
                  , blackKings = [(6,3),(4,3)]
                  , redKings = [(0,1),(0,5)]
                  , status = RedPlayer
                  , message = ""
                  , history = []}



