from test_utils.benchmark_utils import get_test_data_location

from joinbench.benchmark_organiser import BenchmarkOrganiser


class TestBenchmarkOrganiser:
    def test_can_initialise_with_valid_benchmark_data(self):
        test_data_location: str = get_test_data_location()
        assert BenchmarkOrganiser(test_data_location)

    def test_returns_correct_number_of_benchmark_groups(self):
        test_data_location: str = get_test_data_location()
        bo: BenchmarkOrganiser = BenchmarkOrganiser(test_data_location)

        assert len(bo.get_benchmarks()) == 12
