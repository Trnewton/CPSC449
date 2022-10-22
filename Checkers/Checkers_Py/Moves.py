from curses import flash
from gzip import READ
import structs
from typing import List, Tuple

def jump_moves(st: structs.GameState) -> List[structs.Move]:
    pass

def simple_moves(st: structs.GameState) -> List[structs.Move]:
    def good_cord(x:int, y:int):
        if x < 0 or x > 7 or y < 0 or y > 7:
            return False
        if ((x,y) in st.blackPieces or (x,y) in st.blackKings or
            ((x,y) in st.redPieces or (x,y) in st.redKings)):
            return False

        return True

    moves = []
    if st.status == structs.Status.BLACK: # Black turn
        for x, y in st.blackPieces:
            if good_cord(x+1,y-1):
                moves.append((x+1,y-1))
            if good_cord(x+1,y+1):
                moves.append((x+1,y+1))

        for x, y in st.blackKings:
            if good_cord(x+1,y-1):
                moves.append((x+1,y-1))
            if good_cord(x+1,y+1):
                moves.append((x+1,y+1))
            if good_cord(x-1,y-1):
                moves.append((x-1,y-1))
            if good_cord(x-1,y+1):
                moves.append((x-1,y+1))

    elif st.status == structs.Status.RED: # Red turn
        for x, y in st.redPieces:
            if good_cord(x-1,y-1):
                moves.append((x-1,y-1))
            if good_cord(x-1,y+1):
                moves.append((x-1,y+1))

        for x, y in st.redKings:
            if good_cord(x+1,y-1):
                moves.append((x+1,y-1))
            if good_cord(x+1,y+1):
                moves.append((x+1,y+1))
            if good_cord(x-1,y-1):
                moves.append((x-1,y-1))
            if good_cord(x-1,y+1):
                moves.append((x-1,y+1))

    elif st.status == structs.Status.OVER:# Game is over
        pass
    else: # Bad
        pass

    return moves


def moves(st: structs.GameState) -> List[structs.Move]:
    pass