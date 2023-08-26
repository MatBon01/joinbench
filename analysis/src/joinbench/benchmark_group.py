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

    def get_list_of_benchmarks_with_counts(
        self, counts: List[int]
    ) -> List[BenchmarkData]:
        return list(map(lambda count: self.get_benchmark_with_count(count), counts))

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

    def get_group_ordered_by_tuple_count(self):
        return BenchmarkGroup(
            BenchmarkGroup.order_benchmarks_by_tuple_count(self.benchmarks)
        )

    def get_largest_mean_from_experiment_group(self, group: str) -> float:
        return max(
            list(
                map(
                    lambda benchmark: benchmark.get_largest_mean_from_group(group),
                    self.benchmarks,
                )
            )
        )

    def get_query_list(self) -> List[str]:
        # Assume all benchmarks have the same query list
        return self.benchmarks[0].get_query_list()

    def get_standard_deviations(self, query: str, function: str) -> List[float]:
        return list(
            map(
                lambda benchmark: benchmark.get_standard_deviation(query, function),
                self.benchmarks,
            )
        )
