import csv
from typing import List

from .cells.cell import Cell
from .table_generator import TableGenerator


class CSVTableGenerator:
    def __init__(self, table_generator: TableGenerator, path: str):
        self.path: str = path
        self.table_generator: TableGenerator = table_generator

    def generate(self, num_records: int):
        with open(self.path, "w", newline="") as f:
            writer = csv.writer(f, delimiter=",")
            record: List[str]
            for record in self.table_generator.generate(num_records):
                writer.writerow(record)
