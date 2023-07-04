from random import Random
from typing import Callable

from .cell import Cell
from .random_modified_integer_range import RandomModifiedIntegerRangeCell


class RandomIntegerRangeCell(Cell):
    def __init__(self, lower_bound: int, upper_bound: int, random: Random):
        ID_MODIFIER: Callable[[int], int] = lambda x: x
        self.cell: RandomModifiedIntegerRangeCell = RandomModifiedIntegerRangeCell(
            lower_bound, upper_bound, random, ID_MODIFIER
        )

    def generate(self) -> str:
        return self.cell.generate()
