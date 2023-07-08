from test_utils.benchmark_utils import get_test_benchmark_data

from joinbench.benchmark_data import BenchmarkData
from joinbench.benchmark_plotter import BenchmarkPlotter


class TestBenchmarkPlotter:
    def test_can_initialise_with_data(self):
        data: BenchmarkData = get_test_benchmark_data()
        assert BenchmarkPlotter(data)
