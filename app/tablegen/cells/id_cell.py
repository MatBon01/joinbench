from cell import Cell
import random

class IdCell(Cell):
    def generate(self) -> str:
        return str(random.randint(1, 1000))
