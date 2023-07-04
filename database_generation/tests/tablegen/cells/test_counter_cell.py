import pytest

from databasegen.tablegen.cells.counter_cell import CounterCell


class TestCounterCell:
    def test_change_start(self):
        START_UPPER_BOUND: int = 15
        for start in range(0, START_UPPER_BOUND):
            cell = CounterCell(start)
            assert cell.generate() == str(start)
            assert cell.generate() == str(start + 1)

    def test_counting_generation(self):
        BEGIN: int = 0
        cell = CounterCell(BEGIN)

        ITERATIONS: int = 10
        for counter in range(BEGIN, BEGIN + ITERATIONS):
            assert cell.generate() == str(counter)
