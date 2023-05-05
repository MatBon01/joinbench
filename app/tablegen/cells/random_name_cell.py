from cells.cell import Cell
import random

class RandomNameCell(Cell):
    def generate(self) -> str:
        return random.choice(["Alice", "Bob", "Charlie"])
