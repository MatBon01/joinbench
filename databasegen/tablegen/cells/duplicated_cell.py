from random import Random
from typing import Set

from .cell import Cell


class DuplicateCell(Cell):
    """A cell where we want a minimum of repeated values."""

    def __init__(
        self,
        cell_generator: Cell,
        rate: float,
        random: Random,
        strict: bool = False,
    ):
        if strict:
            raise NotImplementedError("Strict duplication has not been implemented yet")

        self.cell_generator: Cell = cell_generator
        self.rate: float = rate
        self.random: Random = random

        self.seen_values: Set[str] = set()

    def generate(self):
        use_existing: bool = self.random.random() < self.rate
        if self.seen_values and use_existing:
            return self.random.choice(tuple(self.seen_values))
        else:
            new_value: str = self.cell_generator.generate()
            self.seen_values.add(new_value)
            return new_value
