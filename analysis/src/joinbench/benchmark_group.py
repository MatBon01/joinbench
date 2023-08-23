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

    @staticmethod
    def load_with_counts(counts: List[int], path: str = ""):
        assert counts
        benchmarks: List[BenchmarkData] = list(
            map(lambda count: BenchmarkData.load_with_count(count, path), counts)
        )

        return BenchmarkGroup(benchmarks)

    def get_tuple_counts(self) -> List[int]:
        return list(map(lambda benchmark: benchmark.get_tuple_count(), self.benchmarks))

    def get_benchmark_with_count(self, count: int) -> BenchmarkData:
        # Assume that there is only one benchmark with the given count
        for benchmark in self.benchmarks:
            if benchmark.get_tuple_count() == count:
                return benchmark

        raise Exception("Could not find benchmark with given tuple count")

    def make_subgroup_with_counts(self, counts: List[int]):
        benchmarks: List[BenchmarkData] = list(
            map(lambda count: self.get_benchmark_with_count(count), counts)
        )

        return BenchmarkGroup(benchmarks)

    @staticmethod
    def order_benchmarks_by_tuple_count(
        benchmarks: List[BenchmarkData],
    ) -> List[BenchmarkData]:
        return sorted(benchmarks, key=lambda benchmark: benchmark.get_tuple_count())
