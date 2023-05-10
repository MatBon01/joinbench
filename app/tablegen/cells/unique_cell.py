from .cell import Cell
from typing import Final, Set

class UniqueCell(Cell):
    def __init__(self, cell: Cell, max_attempts: int = -1):
        # max_attempts = -1 means no limit
        self.cell: Cell = cell
        self.seen_values: Set[str] = set() 
        self.MAX_ATTEMPTS: Final[int] = max_attempts

    def generate(self) -> str:
        curr_attempt: int = 0
        value: str = self.cell.generate()
        while value in self.seen_values and (curr_attempt < self.MAX_ATTEMPTS
                                             or self.MAX_ATTEMPTS == -1):
            value = self.cell.generate()
        self.seen_values.add(value)
        return value
