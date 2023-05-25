from .cell import Cell
from typing import Set

class TrackingCell(Cell):
    def __init__(self, cell: Cell):
        self.cell: Cell = cell
        self._seen: Set[str] = set()

    def generate(self) -> str:
        result: str = self.cell.generate()
        self._seen.add(result)
        return result

    @property
    def seen(self) -> Set[str]:
        return self._seen

