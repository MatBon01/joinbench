from typing import List

import pytest
from test_utils.benchmark_utils import get_joinbench_benchmarks

from joinbench.benchmark_data import BenchmarkData
from joinbench.benchmark_group import BenchmarkGroup


class TestBenchmarkGroup:
    def test_can_initialise_benchmark_group_with_benchmarks(self):
        bs: List[BenchmarkData] = get_joinbench_benchmarks()
        assert BenchmarkGroup(bs)

    def test_can_get_function_name_list(self):
        # Assume that all benchmarks have the same function names
        benchmarks: List[BenchmarkData] = get_joinbench_benchmarks()
        group: BenchmarkGroup = BenchmarkGroup(benchmarks)
        assert set(group.get_function_name_list()) == {
            "old comprehension",
            "modular product",
            "modular indexed",
        }

    def test_cannot_initialise_with_empty_list(self):
        with pytest.raises(AssertionError):
            BenchmarkGroup([])
