import json
from typing import Any, List


class BenchmarkOrganiser:
    def __init__(self, data_path: str):
        with open(data_path, "r") as f:
            self.data = json.load(f)

    def get_benchmarks(self) -> List[Any]:
        return self.data
