from databasegen.tablegen.cells.random_integer_range_cell import \
    RandomIntegerRangeCell


class TestRandomIntegerRangeCell:
    def test_can_supply_range_during_instantiation(self):
        UPPER_BOUND: int = 12
        LOWER_BOUND: int = 3
        assert RandomIntegerRangeCell(LOWER_BOUND, UPPER_BOUND)
