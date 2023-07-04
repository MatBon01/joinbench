from random import Random
from databasegen.tablegen.cells.cell import Cell


class RandomIntegerRangeCell(Cell):
    def __init__(self, lower_bound: int, upper_bound: int, random: Random):
        self.lower_bound = lower_bound
        self.upper_bound = upper_bound
        self.random = random

    def generate(self) -> str:
        return str(self.random.randint(self.lower_bound, self.upper_bound))
