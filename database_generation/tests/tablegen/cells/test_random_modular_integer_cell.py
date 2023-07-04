from random import Random

from test_utils.range_test_helper import RangeTestHelper

from databasegen.tablegen.cells.random_modular_integer_cell import \
    RandomModularIntegerCell
from databasegen.tablegen.cells.random_modular_integer_cell import \
    RandomModularIntegerCell as RandomModularIntegerCell


class TestRandomModularInteger:
    def test_can_supply_modulus_and_random_generator_when_instantiating(self):
        MODULO = 7
        random: Random = Random()
        assert RandomModularIntegerCell(MODULO, random)

    def test_generate_supplies_number_in_range(self):
        TEST_REPEATS: int = 10
        MODULO = 7
        random: Random = Random()
        cell: RandomModularIntegerCell = RandomModularIntegerCell(MODULO, random)
        RangeTestHelper.test_int_within_range(cell, 0, MODULO - 1, TEST_REPEATS)

    def test_generate_changes_number(self):
        MAX_ATTEMPTS = 100

        MODULO = 7
        random: Random = Random()
        cell: RandomModularIntegerCell = RandomModularIntegerCell(MODULO, random)

        RangeTestHelper.test_varied_output(cell, MAX_ATTEMPTS)
