from typing import List

from test_utils.benchmark_utils import get_test_benchmark_data

from joinbench.benchmark_data import BenchmarkData
from joinbench.tuple_count_comparison_plotter import \
    TupleCountComparisonPlotter


class TestTupleCountComparisonPlotter:
    def test_can_construct_with_list_of_benchmark_data(self):
        benchmark_data1: BenchmarkData = get_test_benchmark_data()
        benchmark_data2: BenchmarkData = get_test_benchmark_data()
        benchmark_data3: BenchmarkData = get_test_benchmark_data()

        benchmark_data_list: List[BenchmarkData] = [
            benchmark_data1,
            benchmark_data2,
            benchmark_data3,
        ]

        plotter: TupleCountComparisonPlotter = TupleCountComparisonPlotter(
            benchmark_data_list
        )

        assert plotter
