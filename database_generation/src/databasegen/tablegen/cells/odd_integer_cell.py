from random import Random
from typing import Callable

from databasegen.tablegen.cells.cell import Cell
from databasegen.tablegen.cells.random_modified_modular_integer_cell import \
    RandomModifiedModularIntegerCell


class OddIntegerCell(Cell):
    def __init__(self, num_odd_numbers: int, random: Random):
        ODD_TRANSFORMATION: Callable[[int], int] = lambda x: 2 * x + 1
        self.modified_range_cell: RandomModifiedModularIntegerCell = (
            RandomModifiedModularIntegerCell(
                num_odd_numbers, random, ODD_TRANSFORMATION
            )
        )

    def generate(self) -> str:
        return self.modified_range_cell.generate()
