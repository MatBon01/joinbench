from enum import IntEnum
from random import Random
from typing import Final

from tablegen.cells.cell import Cell

Year = int


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


def is_leap_year(year: Year) -> bool:
    return year % 4 == 0


def num_days_in_month(month: Month, year: Year) -> int:
    if month == Month.FEBRUARY:
        FEBRUARY_DAYS_IN_LEAP_YEAR: Final[int] = 29
        FEBRUARY_DAYS_NO_LEAP_YEAR: Final[int] = 28
        if is_leap_year(year):
            return FEBRUARY_DAYS_IN_LEAP_YEAR
        else:
            return FEBRUARY_DAYS_NO_LEAP_YEAR

    DAYS_IN_MONTH: Final[dict[Month, int]] = {
        Month.JANUARY: 31,
        Month.MARCH: 31,
        Month.APRIL: 30,
        Month.MAY: 31,
        Month.JUNE: 30,
        Month.JULY: 31,
        Month.AUGUST: 31,
        Month.SEPTEMBER: 30,
        Month.OCTOBER: 31,
        Month.NOVEMBER: 30,
        Month.DECEMBER: 31,
    }
    return DAYS_IN_MONTH[month]


class DateCell(Cell):
    def __init__(self, year_lower: Year, year_upper: Year, random: Random) -> None:
        self.year_lower: Year = year_lower
        self.year_upper: Year = year_upper
        self.random: Random = random

    def generate_year(self) -> Year:
        return self.random.randint(self.year_lower, self.year_upper)

    def generate_month(self) -> Month:
        return self.random.choice(list(Month))

    def generate_day(self, month: Month, year: Year) -> int:
        FIRST_DAY: Final[int] = 1
        return self.random.randint(FIRST_DAY, num_days_in_month(month, year))

    def generate(self) -> str:
        year: Year = self.generate_year()
        month: Month = self.generate_month()
        day: int = self.generate_day(month, year)
        return f"{year:04}{month:02}{day:02}"
