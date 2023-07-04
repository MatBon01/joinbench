from databasegen.tablegen.cells.cell import Cell


class RandomIntegerRangeCell(Cell):
    def __init__(self, lower_bound: int, upper_bound: int):
        self.lower_bound = lower_bound
        self.upper_bound = upper_bound
