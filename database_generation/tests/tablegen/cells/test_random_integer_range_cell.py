from random import Random
from typing import List, Tuple

from range_test_helper import RangeTestHelper

from databasegen.tablegen.cells.random_integer_range_cell import \
    RandomIntegerRangeCell


class TestRandomIntegerRangeCell:
    def test_can_supply_range_and_random_object_during_instantiation(self):
        UPPER_BOUND: int = 12
        LOWER_BOUND: int = 3
        random: Random = Random()
        assert RandomIntegerRangeCell(LOWER_BOUND, UPPER_BOUND, random)

    def test_generated_values_within_range(self):
        TEST_REPEATS: int = 10
        cell_ranges: List[Tuple[int, int]] = [(0, 10), (4, 4), (-10, 10)]

        lower_bound: int
        upper_bound: int
        for lower_bound, upper_bound in cell_ranges:
            cell: RandomIntegerRangeCell = RandomIntegerRangeCell(
                lower_bound, upper_bound, Random()
            )
            RangeTestHelper.test_int_within_range(
                cell, lower_bound, upper_bound, TEST_REPEATS
            )

    def test_cell_has_random_output(self):
        # This test has a 1 in 2^1000 chance of failing assuming a uniform
        # distribution
        ATTEMPTS: int = 1000
        LOWER_BOUND: int = 0
        UPPER_BOUND: int = 1
        random: Random = Random()
        cell: RandomIntegerRangeCell = RandomIntegerRangeCell(
            LOWER_BOUND, UPPER_BOUND, random
        )
        RangeTestHelper.test_varied_output(cell, ATTEMPTS)
