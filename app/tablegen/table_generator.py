from .record_generator import RecordGenerator
from typing import List, Iterable

class table_generator:
    def __init__(self, record_generator: RecordGenerator, num_of_records: int):
        self.record_generator: RecordGenerator = record_generator
        self.num_of_records: int = num_of_records

    def generate(self, num_records: int = 1) -> Iterable[List[str]]:
        for _ in range(num_records):
            yield self.record_generator.generate()
