from random import Random
from typing import Callable

from test_utils.range_test_helper import RangeTestHelper

from databasegen.tablegen.cells.random_modified_modular_integer_cell import \
    RandomModifiedModularIntegerCell


class TestRandomModifiedModularInteger:
    def test_can_initialise_with_all_args(self):
        """
        Expecting to supply a modulus, random number generator and modifier
        """
        MODULUS: int = 17
        random: Random = Random()
        ID_FUNC: Callable[[int], int] = lambda x: x
        assert RandomModifiedModularIntegerCell(MODULUS, random, ID_FUNC)

    def test_generate_returns_integers_within_range_when_no_modification(self):
        TEST_REPETITIONS: int = 100

        MODULUS: int = 17
        random: Random = Random()
        ID_FUNC: Callable[[int], int] = lambda x: x
        cell: RandomModifiedModularIntegerCell = RandomModifiedModularIntegerCell(
            MODULUS, random, ID_FUNC
        )

        RangeTestHelper.test_int_within_range(cell, 0, 17, TEST_REPETITIONS)

    def test_generate_varies_result_with_no_modification(self):
        MAX_ATTEMPTS = 1000
        MODULUS: int = 2
        # assuming uniform distribution there is a 1 in 2^1000 chance the test
        # fails

        random: Random = Random()
        ID_FUNC: Callable[[int], int] = lambda x: x
        cell: RandomModifiedModularIntegerCell = RandomModifiedModularIntegerCell(
            MODULUS, random, ID_FUNC
        )

        RangeTestHelper.test_varied_output(cell, MAX_ATTEMPTS)

    def test_generates_within_modified_range(self):
        MAX_ATTEMPTS: int = 1000
        MODULUS: int = 2
        times_two: Callable[[int], int] = lambda x: x * 2
        # Will test at least one value is equal to 2 as 1 is only non zero
        # value and the result must be doubled
        # There is a 1 in 2^1000 chance the test fails assuming

        random: Random = Random()
        cell: RandomModifiedModularIntegerCell = RandomModifiedModularIntegerCell(
            MODULUS, random, times_two
        )

        i: int = 0
        while i < MAX_ATTEMPTS and cell.generate() != str((MODULUS - 1) * 2):
            i += 1

        assert i < MAX_ATTEMPTS
