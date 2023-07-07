import json
from typing import Dict, List, Set, Tuple

from test_utils.benchmark_utils import (
    get_list_of_data_report_names,
    get_test_data_benchmark_group_and_benchmark_mapping,
    get_test_data_benchmark_group_names, get_test_data_location,
    get_test_data_report_names)

from joinbench.benchmark_organiser import BenchmarkOrganiser


class TestBenchmarkOrganiser:
    def test_can_initialise_with_valid_benchmark_data(self):
        test_data_location: str = get_test_data_location()
        assert BenchmarkOrganiser(test_data_location)

    def test_can_correctly_separate_report_name(self):
        assert BenchmarkOrganiser.separate_benchmark_group_and_name(
            "group/benchmark"
        ) == (
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

    def test_can_correctly_map_correct_number_of_benchmark_groups(self):
        names: List[str] = get_list_of_data_report_names()
        groups = BenchmarkOrganiser.map_benchmark_groups_and_benchmark_indices(
            names
        ).keys()
        assert len(groups) == 4

    def test_can_correctly_find_benchmark_groups(self):
        names: List[str] = get_list_of_data_report_names()
        groups = BenchmarkOrganiser.map_benchmark_groups_and_benchmark_indices(names)
        expected: Set[str] = get_test_data_benchmark_group_names()
        assert set(groups) == expected

    def test_can_correctly_find_benchmark_indices(self):
        expected: Dict[
            str, List[Tuple[int, str]]
        ] = get_test_data_benchmark_group_and_benchmark_mapping()

        names: List[str] = get_list_of_data_report_names()
        actual: Dict[
            str, List[Tuple[int, str]]
        ] = BenchmarkOrganiser.map_benchmark_groups_and_benchmark_indices(names)

        assert actual == expected

    def test_can_get_correct_benchmark_group_names(self):
        expected: Set[str] = get_test_data_benchmark_group_names()

        names: List[str] = get_list_of_data_report_names()
        actual: Set[str] = BenchmarkOrganiser.get_benchmark_group_names(names)

        assert actual == expected
