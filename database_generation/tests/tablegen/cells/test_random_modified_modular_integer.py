from random import Random
from typing import Callable

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
