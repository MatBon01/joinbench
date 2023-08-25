from test_utils.benchmark_utils import get_joinbench_benchmark_group

from joinbench.benchmark_group import BenchmarkGroup
from joinbench.benchmark_group_plotter import BenchmarkGroupPlotter


class TestBenchmarkGroupPlotter:
    def test_can_initialise_with_benchmark_group(self):
        group: BenchmarkGroup = get_joinbench_benchmark_group()
        assert BenchmarkGroupPlotter(group)
