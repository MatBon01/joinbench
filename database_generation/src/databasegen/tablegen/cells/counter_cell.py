from databasegen.tablegen.cells.cell import Cell

class CounterCell(Cell):
    def __init__(self, start: int = 0):
        self.start = start

    def generate(self):
        curr: int = self.start
        self.start += 1
        return str(curr)
