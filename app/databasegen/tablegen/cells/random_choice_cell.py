from random import Random
from typing import List

from .cell import Cell


class RandomChoiceCell(Cell):
    def __init__(self, strs: List[str], random: Random):
        self.random: Random = random
        self.strs: List[str] = strs

    def generate(self) -> str:
        return self.random.choice(self.strs)
