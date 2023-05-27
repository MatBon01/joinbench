from enum import IntEnum
from random import Random

from tablegen.cells.cell import Cell


class Month(IntEnum):
    JANUARY = 1
    FEBRUARY = 2
    MARCH = 3
    APRIL = 4
    MAY = 5
    JUNE = 6
    JULY = 7
    AUGUST = 8
    SEPTEMBER = 9
    OCTOBER = 10
    NOVEMBER = 11
    DECEMBER = 12


Year = int


class DateCell(Cell):
    def __init__(self, year_lower: Year, year_upper: Year, random: Random) -> None:
        self.year_lower: Year = year_lower
        self.year_upper: Year = year_upper
        self.random: Random = random

    def generate_year(self) -> Year:
        return self.random.randint(self.year_lower, self.year_upper)

    def generate_month(self) -> Month:
        return self.random.choice(list(Month))

    def generate_day(self, month: Month, year: Year) -> str:
        raise NotImplementedError()

    def generate(self) -> str:
        raise NotImplementedError("Not all components of date generated")
