from random import Random

from tablegen.cells.cell import Cell


class DateCell(Cell):
    def __init__(self, year_lower: int, year_upper: int, random: Random) -> None:
        self.year_lower: int = year_lower
        self.year_upper: int = year_upper
        self.random: Random = random

    def generate_year(self) -> str:
        return str(self.random.randint(self.year_lower, self.year_upper))

    def generate_month(self) -> str:
        # Assume each month is an integer from 1 to 12
        FIRST_MONTH: int = 1
        LAST_MONTH: int = 12
        return str(self.random.randint(FIRST_MONTH, LAST_MONTH))

    def generate(self) -> str:
        raise NotImplementedError("Not all components of date generated")
