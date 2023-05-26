from random import Random

from tablegen.cells.cell import Cell


class AmountCell(Cell):
    def __init__(self, random: Random, lower: float = 0.01, upper: float = 9999.99):
        self.random: Random = random
        self.lower: float = lower
        self.upper: float = upper

    def generate(self) -> str:
        return str(self.random.uniform(self.lower, self.upper))
