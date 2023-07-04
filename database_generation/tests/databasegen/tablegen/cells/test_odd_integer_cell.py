from random import Random

from test_utils.range_test_helper import RangeTestHelper

from databasegen.tablegen.cells.odd_integer_cell import OddIntegerCell


class TestOddIntegerCell:
    def test_can_initialise_odd_integer(self):
        NUM_VALUES: int = 5
        random: Random = Random()
        assert OddIntegerCell(NUM_VALUES, random)

    def test_generates_odd_number(self):
        TEST_REPETITIONS: int = 100
        NUM_VALUES: int = 15
        random: Random = Random()
        cell: OddIntegerCell = OddIntegerCell(NUM_VALUES, random)

        for _ in range(TEST_REPETITIONS):
            assert int(cell.generate()) % 2 == 1

    def test_generates_varied_input(self):
        MAX_ATTEMPTS: int = 1000
        NUM_VALUES: int = 2
        random: Random = Random()
        cell: OddIntegerCell = OddIntegerCell(NUM_VALUES, random)
        # there is a 1 in 2^1000 chance the test fails assuming a uniform
        # distribution

        RangeTestHelper.test_varied_output(cell, MAX_ATTEMPTS)
