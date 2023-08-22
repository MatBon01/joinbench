from typing import List

import pytest
from test_utils.benchmark_utils import (get_joinbench_benchmark_group,
                                        get_joinbench_benchmarks,
                                        get_joinbench_test_data_directory_path)

from joinbench.benchmark_data import BenchmarkData
from joinbench.benchmark_group import BenchmarkGroup


class TestBenchmarkGroup:
    def test_can_initialise_benchmark_group_with_benchmarks(self):
        bs: List[BenchmarkData] = get_joinbench_benchmarks()
        assert BenchmarkGroup(bs)

    def test_can_get_function_name_list(self):
        # Assume that all benchmarks have the same function names
        group: BenchmarkGroup = get_joinbench_benchmark_group()
        assert set(group.get_function_name_list()) == {
            "old comprehension",
            "modular product",
            "modular indexed",
        }

    def test_cannot_initialise_with_empty_list(self):
        with pytest.raises(AssertionError):
            BenchmarkGroup([])

    def test_can_get_mean_for_benchmark_group(self):
        group: BenchmarkGroup = get_joinbench_benchmark_group()
        actual_means: List[float] = group.get_benchmark_means(
            "join on onePercent", "modular indexed"
        )
        expected_means: List[float] = [0.002630475582435891, 0.002630475582435891]
        # Must protect against floating point errors
        for index, mean in enumerate(actual_means):
            assert (mean - expected_means[index]) < 0.000000000000001

    def test_can_create_group_with_multiple_tuple_counts(self):
        path: str = get_joinbench_test_data_directory_path()
        group: BenchmarkGroup = BenchmarkGroup.load_with_counts([127, 1000], path=path)
        assert len(group.benchmarks) == 2
        INDEX_127: int = 0
        INDEX_1000: int = 1
        assert group.benchmarks[INDEX_127].get_tuple_count() == 127
        assert group.benchmarks[INDEX_1000].get_tuple_count() == 1000

    def test_cannot_load_with_empty_list_of_tuple_counts(self):
        with pytest.raises(AssertionError):
            BenchmarkGroup.load_with_counts([], path="")

    def test_can_get_tuple_count_for_benchmarks(self):
        group: BenchmarkGroup = get_joinbench_benchmark_group()
        assert group.get_tuple_counts() == [127, 1000]

    def test_can_get_benchmark_by_tuple_count(self):
        group: BenchmarkGroup = get_joinbench_benchmark_group()
        assert group.get_benchmark_with_count(1000).get_tuple_count() == 1000
        assert group.get_benchmark_with_count(127).get_tuple_count() == 127

    def test_raises_an_exceptioin_if_tuple_count_not_in_group(self):
        group: BenchmarkGroup = get_joinbench_benchmark_group()
        with pytest.raises(Exception):
            # Where the count is not in the group
            group.get_benchmark_with_count(10)
