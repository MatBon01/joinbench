from typing import Final, Set

from .cell import Cell


class UniqueCell(Cell):
    def __init__(self, cell: Cell, max_attempts: int = -1):
        # max_attempts = -1 means no limit
        self.cell: Cell = cell
        self.seen_values: Set[str] = set()
        self.MAX_ATTEMPTS: Final[int] = max_attempts

    def has_remaining_attempts(self, curr_attempt: int) -> bool:
        return self.MAX_ATTEMPTS == -1 or curr_attempt < self.MAX_ATTEMPTS

    def generate(self) -> str:
        curr_attempt: int = 0
        value: str = self.cell.generate()
        while value in self.seen_values and self.has_remaining_attempts(curr_attempt):
            value = self.cell.generate()
        self.seen_values.add(value)
        return value
