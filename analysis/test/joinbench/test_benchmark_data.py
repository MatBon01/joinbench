from typing import Dict, List, Set, Tuple

from test_utils.benchmark_utils import (
    get_list_of_data_report_names, get_test_benchmark_data,
    get_test_data_benchmark_group_and_benchmark_mapping,
    get_test_data_benchmark_group_names, get_test_data_location)

from joinbench.benchmark_data import BenchmarkData


class TestBenchmarkData:
    def test_can_initialise_with_valid_benchmark_data(self):
        test_data_location: str = get_test_data_location()
        assert BenchmarkData(test_data_location)

    def test_can_get_report_names(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()
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
        benchmark_data: BenchmarkData = get_test_benchmark_data()

        groups = benchmark_data.map_benchmark_groups_and_benchmark_indices().keys()
        assert len(groups) == 4

    def test_can_correctly_find_benchmark_groups(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()

        groups = benchmark_data.map_benchmark_groups_and_benchmark_indices()
        expected: Set[str] = get_test_data_benchmark_group_names()

        assert set(groups) == expected

    def test_can_correctly_find_benchmark_indices(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()

        expected: Dict[
            str, Set[Tuple[int, str]]
        ] = get_test_data_benchmark_group_and_benchmark_mapping()

        actual: Dict[
            str, Set[Tuple[int, str]]
        ] = benchmark_data.map_benchmark_groups_and_benchmark_indices()

        assert actual == expected

    def test_can_get_correct_benchmark_group_names(self):
        test_data_location: str = get_test_data_location()
        benchmark_data: BenchmarkData = BenchmarkData(test_data_location)

        expected: Set[str] = get_test_data_benchmark_group_names()

        actual: Set[str] = benchmark_data.get_benchmark_group_names()

        assert actual == expected

    def test_can_retrieve_mean_for_a_benchmark(self):
        test_data_location: str = get_test_data_location()
        benchmark_data: BenchmarkData = BenchmarkData(test_data_location)

        expected: float = 0.000000029314520320675693
        assert benchmark_data.get_benchmark_mean(1) == expected

    def test_can_retrieve_multiple_means(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()

        expected: List[float] = [0.00000003142288461705547, 0.000000029314520320675693]

        assert benchmark_data.get_means_of_benchmark_list([0, 1]) == expected
