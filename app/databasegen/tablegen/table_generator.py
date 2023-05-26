from typing import Iterable, List

from .record_generator import RecordGenerator


class TableGenerator:
    def __init__(self, record_generator: RecordGenerator):
        self.record_generator: RecordGenerator = record_generator

    def generate(self, num_records: int = 1) -> Iterable[List[str]]:
        for _ in range(num_records):
            yield self.record_generator.generate()
