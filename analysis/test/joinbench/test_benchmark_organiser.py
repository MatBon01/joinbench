from joinbench.benchmark_organiser import BenchmarkOrganiser


class TestBenchmarkOrganiser:
    def test_can_initialise_with_valid_benchmark_data(self):
        assert BenchmarkOrganiser("test/joinbench/test_data.json")

    def test_returns_correct_number_of_benchmark_groups(self):
        bo: BenchmarkOrganiser = BenchmarkOrganiser("test/joinbench/test_data.json")

        assert len(bo.get_benchmarks()) == 12
