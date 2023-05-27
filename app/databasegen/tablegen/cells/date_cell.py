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


class DateCell(Cell):
    def __init__(self, year_lower: int, year_upper: int, random: Random) -> None:
        self.year_lower: int = year_lower
        self.year_upper: int = year_upper
        self.random: Random = random

    def generate_year(self) -> str:
        return str(self.random.randint(self.year_lower, self.year_upper))

    def generate_month(self) -> str:
        return str(self.random.randint(Month.JANUARY, Month.DECEMBER))

    def generate_day(self, month: int, year: int) -> str:
        raise NotImplementedError()

    def generate(self) -> str:
        raise NotImplementedError("Not all components of date generated")
