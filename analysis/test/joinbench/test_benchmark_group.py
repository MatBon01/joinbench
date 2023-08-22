from typing import List

import pytest
from test_utils.benchmark_utils import (get_joinbench_benchmark_group,
                                        get_joinbench_benchmarks)

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
