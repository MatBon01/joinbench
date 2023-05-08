from cells.cell import Cell
import random
from typing import List

class RandomNameCell(Cell):
    def __init__(self, names: List[str]):
        self.names: List[str] = names

    def generate(self) -> str:
        return random.choice(self.names)
