from typing import Dict, List, Set, Tuple

from test_utils.benchmark_utils import (
    get_list_of_data_report_names,
    get_test_data_benchmark_group_and_benchmark_mapping,
    get_test_data_benchmark_group_names, get_test_data_location,
    get_test_data_report_names)

from joinbench.benchmark_data import BenchmarkData


class TestBenchmarkData:
    def test_can_initialise_with_valid_benchmark_data(self):
        test_data_location: str = get_test_data_location()
        assert BenchmarkData(test_data_location)

    def test_can_get_report_names(self):
        test_data_location: str = get_test_data_location()
        benchmark_data: BenchmarkData = BenchmarkData(test_data_location)
        assert (
            benchmark_data.get_report_names_in_order()
            == get_list_of_data_report_names()
        )

    def test_can_correctly_separate_report_name(self):
        assert BenchmarkData.separate_benchmark_group_and_name("group/benchmark") == (
            "group",
            "benchmark",
        )

    def test_can_correctly_map_correct_number_of_benchmark_groups(self):
        test_data_location: str = get_test_data_location()
        benchmark_data: BenchmarkData = BenchmarkData(test_data_location)

        groups = benchmark_data.map_benchmark_groups_and_benchmark_indices().keys()
        assert len(groups) == 4

    def test_can_correctly_find_benchmark_groups(self):
        test_data_location: str = get_test_data_location()
        benchmark_data: BenchmarkData = BenchmarkData(test_data_location)

        groups = benchmark_data.map_benchmark_groups_and_benchmark_indices()
        expected: Set[str] = get_test_data_benchmark_group_names()

        assert set(groups) == expected

    def test_can_correctly_find_benchmark_indices(self):
        test_data_location: str = get_test_data_location()
        benchmark_data: BenchmarkData = BenchmarkData(test_data_location)

        expected: Dict[
            str, List[Tuple[int, str]]
        ] = get_test_data_benchmark_group_and_benchmark_mapping()

        actual: Dict[
            str, List[Tuple[int, str]]
        ] = benchmark_data.map_benchmark_groups_and_benchmark_indices()

        assert actual == expected

    def test_can_get_correct_benchmark_group_names(self):
        test_data_location: str = get_test_data_location()
        benchmark_data: BenchmarkData = BenchmarkData(test_data_location)

        expected: Set[str] = get_test_data_benchmark_group_names()

        names: List[str] = get_list_of_data_report_names()
        actual: Set[str] = benchmark_data.get_benchmark_group_names()

        assert actual == expected
