from random import Random
from typing import Callable

from databasegen.tablegen.cells.cell import Cell


class RandomModifiedModularIntegerCell(Cell):
    def __init__(self, modulus: int, random: Random, modifier: Callable[[int], int]):
        self.modulus: int = modulus
        self.random: Random = random
        self.modifier: Callable[[int], int] = modifier

    def generate(self) -> str:
        return str(self.modifier(self.random.randint(0, self.modulus - 1)))
