from random import Random
from typing import Callable

from databasegen.tablegen.cells.cell import Cell


class RandomModifiedIntegerRangeCell(Cell):
    def __init__(
        self,
        lower_bound: int,
        upper_bound: int,
        random: Random,
        modifier: Callable[[int], int],
    ):
        self.lower_bound: int = lower_bound
        self.upper_bound: int = upper_bound
        self.random: Random = random
        self.modifier: Callable[[int], int] = modifier

    def generate(self) -> str:
        return str(
            self.modifier(self.random.randint(self.lower_bound, self.upper_bound))
        )
