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

    def test_generate_supplies_number_in_range(self):
        MODULO = 7
        random: Random = Random()
        cell: RandomModularIntegerCell = RandomModularIntegerCell(MODULO, random)
        value = int(cell.generate())
        assert value >= 0
        assert value < MODULO

    def test_generate_changes_number(self):
        MAX_ATTEMPTS = 100

        MODULO = 7
        random: Random = Random()
        cell: RandomModularIntegerCell = RandomModularIntegerCell(MODULO, random)

        last = ""
        curr = cell.generate()
        i: int = 0
        while i < MAX_ATTEMPTS and (last == "" or curr == last):
            last = curr
            curr = cell.generate()
            i += 1

        assert i < MAX_ATTEMPTS
