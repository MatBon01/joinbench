from typing import List

from .cells.cell import Cell


class RecordGenerator:
    def __init__(self, cells: List[Cell]):
        self.cells: List[Cell] = cells

    def generate(self) -> List[str]:
        return list(map(lambda cell: cell.generate(), self.cells))
