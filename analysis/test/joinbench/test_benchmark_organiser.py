import json

from test_utils.benchmark_utils import (get_test_data_location,
                                        get_test_data_report_names)

from joinbench.benchmark_organiser import BenchmarkOrganiser


class TestBenchmarkOrganiser:
    def test_can_initialise_with_valid_benchmark_data(self):
        test_data_location: str = get_test_data_location()
        assert BenchmarkOrganiser(test_data_location)

    def test_can_correctly_separate_report_name(self):
        assert BenchmarkOrganiser._separate_group_from_name("group/benchmark") == (
            "group",
            "benchmark",
        )

    def test_can_correctly_get_report_names(self):
        test_data_location: str = get_test_data_location()
        with open(test_data_location, "r") as f:
            data = json.load(f)
        assert (
            BenchmarkOrganiser._get_report_names(data) == get_test_data_report_names()
        )
