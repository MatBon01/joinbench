from .cells.cell import Cell
from typing import List

class TableGenerator:
    def __init__(self, cells: List[Cell]):
        self.cells: List[Cell] = cells

    def generate_table(self, num_of_records: int):
        raise NotImplementedError("generate_table has not been implemented yet")
