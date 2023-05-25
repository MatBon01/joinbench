from .cell import Cell
import random
from typing import List

class RandomChoiceCell(Cell):
    def __init__(self, strs: List[str]):
        self.strs: List[str] = strs

    def generate(self) -> str:
        return random.choice(self.strs)
