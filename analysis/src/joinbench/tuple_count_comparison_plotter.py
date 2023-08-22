from typing import List

from .benchmark_data import BenchmarkData


class TupleCountComparisonPlotter:
    def __init__(self, benchmark_data_list: List[BenchmarkData]):
        self.benchmark_data_list = benchmark_data_list
