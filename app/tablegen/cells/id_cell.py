from .cell import Cell
import random

class IdCell(Cell):
    def __init__(self, lower_bound: int = 10000000, upper_bound: int = 99999999):
        self.lower_bound: int = lower_bound
        self.upper_bound: int = upper_bound

    def generate_id(self) -> int:
        return random.randint(self.lower_bound, self.upper_bound)

    def generate(self) -> str:
        return str(self.generate_id())
