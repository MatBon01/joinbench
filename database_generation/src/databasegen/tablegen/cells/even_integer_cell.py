from random import Random
from typing import Callable

from databasegen.tablegen.cells.cell import Cell
from databasegen.tablegen.cells.random_modified_modular_integer_cell import \
    RandomModifiedModularIntegerCell


class EvenIntegerCell(Cell):
    def __init__(self, num_even_numbers: int, random: Random):
        EVEN_TRANSFORMATION: Callable[[int], int] = lambda x: x * 2
        self.modified_range_cell: Cell = RandomModifiedModularIntegerCell(
            num_even_numbers, random, EVEN_TRANSFORMATION
        )

    def generate(self) -> str:
        return self.modified_range_cell.generate()
