from random import Random
from typing import Callable

from databasegen.tablegen.cells.cell import Cell
from databasegen.tablegen.cells.random_modified_integer_range import \
    RandomModifiedIntegerRangeCell


class RandomModifiedModularIntegerCell(Cell):
    def __init__(self, modulus: int, random: Random, modifier: Callable[[int], int]):
        self.range_cell: RandomModifiedIntegerRangeCell = (
            RandomModifiedIntegerRangeCell(0, modulus - 1, random, modifier)
        )

    def generate(self) -> str:
        return str(self.range_cell.generate())
