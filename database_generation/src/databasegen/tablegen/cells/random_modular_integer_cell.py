from random import Random

from databasegen.tablegen.cells.cell import Cell
from databasegen.tablegen.cells.random_integer_range_cell import \
    RandomIntegerRangeCell


class RandomModularIntegerCell(Cell):
    def __init__(self, modulo: int, random: Random):
        self.range: RandomIntegerRangeCell = RandomIntegerRangeCell(
            0, modulo - 1, random
        )
