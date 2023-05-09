from .cells.cell import Cell
from typing import List

class RecordGenerator:
    def __init__(self, cells: List[Cell]):
        self.cells = cells

    def generate(self) -> List[str]:
        return list(map(lambda cell: cell.generate(), self.cells))
