from random import Random
from typing import Callable

from databasegen.tablegen.cells.cell import Cell


class RandomModifiedModularIntegerCell(Cell):
    def __init__(self, modulus: int, random: Random, modifier: Callable[[int], int]):
        self.modulus: int = modulus
        self.random: Random = random
        self.modifier: Callable[[int], int] = modifier
