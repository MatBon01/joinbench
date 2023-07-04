from databasegen.tablegen.cells.cell import Cell


class RangeTestHelper:
    @staticmethod
    def test_int_within_range(
        cell: Cell, lower_bound: int, upper_bound: int, test_repeats: int
    ) -> None:
        for _ in range(test_repeats):
            value = int(cell.generate())
            assert value >= lower_bound
            assert value <= upper_bound

    @staticmethod
    def test_varied_output(cell: Cell, max_attempts: int) -> None:
        last = ""
        curr = cell.generate()
        i: int = 0
        while i < max_attempts and (last == "" or curr == last):
            last = curr
            curr = cell.generate()
            i += 1

        assert i < max_attempts
