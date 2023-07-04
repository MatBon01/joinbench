from random import Random

from databasegen.tablegen.cells.random_modular_integer_cell import \
    RandomModularIntegerCell
from databasegen.tablegen.cells.random_modular_integer_cell import \
    RandomModularIntegerCell as RandomModularIntegerCell


class TestRandomModularInteger:
    def test_can_supply_modulus_and_random_generator_when_instantiating(self):
        MODULO = 7
        random: Random = Random()
        assert RandomModularIntegerCell(MODULO, random)
