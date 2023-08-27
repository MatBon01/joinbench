from random import Random
from typing import Callable

from .cell import Cell
from .random_modified_modular_integer_cell import \
    RandomModifiedModularIntegerCell


class RandomModularIntegerCell(Cell):
    def __init__(self, modulo: int, random: Random):
        ID_FUNC: Callable[[int], int] = lambda x: x

        self.range_cell: RandomModifiedModularIntegerCell = (
            RandomModifiedModularIntegerCell(modulo, random, ID_FUNC)
        )

    def generate(self) -> str:
        return self.range_cell.generate()
