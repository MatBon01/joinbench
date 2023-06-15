import csv
from typing import List

from .cells.cell import Cell
from .table_generator import TableGenerator


class CSVTableGenerator:
    def __init__(
        self, table_generator: TableGenerator, path: str, dialect: str = "excel"
    ):
        self.path: str = path
        self.table_generator: TableGenerator = table_generator
        self.dialect: str = dialect

    def generate(self, num_records: int):
        with open(self.path, "w", newline="") as f:
            writer = csv.writer(f, delimiter=",", dialect=self.dialect)
            record: List[str]
            for record in self.table_generator.generate(num_records):
                writer.writerow(record)
