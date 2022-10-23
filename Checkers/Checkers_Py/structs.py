from dataclasses import dataclass
from enum import Enum
from typing import Set, Tuple, List


class Status(Enum):
    RED = 0
    BLACK = 1
    OVER = 2

class Piece(Enum):
    PAWN = 0
    KING = 1

@dataclass
class Coord:
    porK: Piece
    x: int
    y: int

@dataclass
class GameState:
    blackPieces: Set[Tuple[int, int]]
    redPieces: Set[Tuple[int, int]]
    blackKings: Set[Tuple[int, int]]
    redKings: Set[Tuple[int, int]]
    status: "Status"
    message: str
    histroy: List[List[Coord]]

