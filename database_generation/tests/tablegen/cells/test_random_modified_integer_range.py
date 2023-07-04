from random import Random
from typing import Callable

from databasegen.tablegen.cells.random_modified_integer_range import \
    RandomModifiedIntegerRangeCell


class TestRandomModifiedIntegerRangeCell:
    def test_can_supply_all_arguments_when_instantiating(self):
        """
        We expect to be able to supply the range, a random number generator and
        a modifying function
        """
        LOWER_BOUND: int = 0
        UPPER_BOUND: int = 10
        random: Random = Random()
        modifier: Callable[[int], int] = lambda x: x
        assert RandomModifiedIntegerRangeCell(
            LOWER_BOUND, UPPER_BOUND, random, modifier
        )
