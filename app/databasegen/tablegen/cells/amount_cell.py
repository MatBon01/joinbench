from random import Random

from tablegen.cells.cell import Cell


Amount = int

class AmountCell(Cell):
    def __init__(self, random: Random, lower: Amount = 1, upper: Amount = 999999):
        self.random: Random = random
        self.lower: Amount = lower
        self.upper: Amount = upper

    def generate(self) -> str:
        return str(self.random.randint(self.lower, self.upper))
