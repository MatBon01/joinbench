from typing import List

from .benchmark_data import BenchmarkData


class BenchmarkGroup:
    def __init__(self, benchmarks: List[BenchmarkData]):
        assert benchmarks
        self.benchmarks: List[BenchmarkData] = benchmarks

    def get_function_name_list(self) -> List[str]:
        # Assume that all benchmarks have the same function name list
        return self.benchmarks[0].get_function_name_list()

    def get_benchmark_means(self, group: str, function_name: str) -> List[float]:
        return list(
            map(
                lambda benchmark: benchmark.get_benchmark_mean(group, function_name),
                self.benchmarks,
            )
        )
