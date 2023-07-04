from random import Random

from databasegen.tablegen.cells.even_integer_cell import EvenIntegerCell
from test_utils.range_test_helper import RangeTestHelper


class TestRandomEvenIntegerCell:
    def test_can_initialise_with_num_values_and_random_generator(self):
        NUM_VALUES: int = 5
        random: Random = Random()
        assert EvenIntegerCell(NUM_VALUES, random)

    def test_generated_values_even(self):
        NUM_VALUES: int = 15
        TEST_REPETITIONS: int = 100
        random: Random = Random()
        cell: EvenIntegerCell = EvenIntegerCell(NUM_VALUES, random)

        for _ in range(TEST_REPETITIONS):
            assert int(cell.generate()) % 2 == 0

    def test_generated_values_varied(self):
        MAX_ATTEMPTS: int = 1000
        NUM_VALUES: int = 15
        random: Random = Random()
        cell: EvenIntegerCell = EvenIntegerCell(NUM_VALUES, random)

        RangeTestHelper.test_varied_output(cell, MAX_ATTEMPTS)
