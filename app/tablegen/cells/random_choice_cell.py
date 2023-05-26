import random
from typing import List

from .cell import Cell


class RandomChoiceCell(Cell):
    def __init__(self, strs: List[str]):
        self.strs: List[str] = strs

    def generate(self) -> str:
        return random.choice(self.strs)
