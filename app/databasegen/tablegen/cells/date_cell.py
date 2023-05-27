from random import Random

from tablegen.cells.cell import Cell


class DateCell(Cell):
    def __init__(self, year_lower: int, year_upper: int, random: Random) -> None:
        self.year_lower: int = year_lower
        self.year_upper: int = year_upper
        self.random: Random = random

    def generate(self) -> str:
        raise NotImplementedError("Not all components of date generated")
