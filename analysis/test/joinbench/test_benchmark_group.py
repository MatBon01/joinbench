from typing import List

from test_utils.benchmark_utils import get_test_benchmarks

from joinbench.benchmark import Benchmark
from joinbench.benchmark_group import BenchmarkGroup


class TestBenchmarkGroup:
    def test_can_initialise_benchmark_group_with_benchmarks(self):
        bs: List[Benchmark] = get_test_benchmarks()
        assert BenchmarkGroup(bs)
