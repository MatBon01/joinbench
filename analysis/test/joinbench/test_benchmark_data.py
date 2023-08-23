from typing import Dict, List, Set, Tuple

import pytest
from test_utils.benchmark_utils import (
    compare_floats, get_function_names, get_joinbench_test_data_directory_path,
    get_list_of_data_report_names, get_test_benchmark_data,
    get_test_benchmark_data_number_of_tuples,
    get_test_data_benchmark_group_and_benchmark_mapping,
    get_test_data_benchmark_group_names, get_test_data_location)

from joinbench.benchmark_data import BenchmarkData


class TestBenchmarkData:
    def test_can_initialise_with_valid_benchmark_data_and_tuple_number(self):
        test_data_location: str = get_test_data_location()
        number_of_tuples: int = get_test_benchmark_data_number_of_tuples()
        assert BenchmarkData(test_data_location, number_of_tuples)

    def test_can_get_report_names(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()
        assert (
            benchmark_data.get_report_names_in_order()
            == get_list_of_data_report_names()
        )

    def test_can_correctly_separate_report_name(self):
        assert BenchmarkData.separate_benchmark_group_and_function(
            "group/benchmark"
        ) == (
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
            str, List[Tuple[int, str]]
        ] = get_test_data_benchmark_group_and_benchmark_mapping()

        actual: Dict[
            str, List[Tuple[int, str]]
        ] = benchmark_data.map_benchmark_groups_and_benchmark_indices()

        assert actual == expected

    def test_can_get_correct_benchmark_group_names(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()

        expected: Set[str] = get_test_data_benchmark_group_names()

        actual: Set[str] = benchmark_data.get_benchmark_group_names()

        assert actual == expected

    def test_can_retrieve_mean_for_a_benchmark_from_index(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()

        expected: float = 0.000000029314520320675693
        assert benchmark_data.get_benchmark_mean_from_index(1) == expected

    def test_can_retrieve_mean_from_experiment_informxtion(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()
        expected: float = 0.000000029314520320675693
        assert (
            benchmark_data.get_benchmark_mean("join on onePercent", "old comprehension")
            == expected
        )

    def test_can_retrieve_multiple_means(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()

        expected: List[float] = [0.00000003142288461705547, 0.000000029314520320675693]

        assert benchmark_data.get_means_of_benchmark_list([0, 1]) == expected

    @pytest.mark.parametrize(
        "name,expected", [("joinbench127.json", 127), ("joinbench1000.json", 1000)]
    )
    def test_can_infer_tuple_count_from_path(self, name, expected):
        name = get_joinbench_test_data_directory_path() + name
        benchmark_data: BenchmarkData = BenchmarkData.infer_tuple_count_from_path(name)
        assert benchmark_data.get_tuple_count() == expected

    @pytest.mark.parametrize("count", [127, 1000])
    def test_can_get_tuple_count(self, count):
        benchmark_data_path: str = get_test_data_location()
        benchmark_data: BenchmarkData = BenchmarkData(benchmark_data_path, count)
        assert benchmark_data.get_tuple_count() == count

    def test_can_work_out_benchmark_index_from_group_and_function_name(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()
        assert (
            benchmark_data.get_benchmark_index(
                "join onePercent and twentyPercent", "old comprehension"
            )
            == 4
        )

    def test_can_get_function_name_from_experiment(self):
        assert (
            BenchmarkData.get_function_name_from_experiment("group/function")
            == "function"
        )

    def test_can_get_a_list_of_function_names(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()
        assert set(benchmark_data.get_function_name_list()) == set(get_function_names())

    def test_loading_with_count(self):
        path: str = get_joinbench_test_data_directory_path()
        benchmark127: BenchmarkData = BenchmarkData.load_with_count(127, path=path)
        assert benchmark127.get_tuple_count() == 127
        benchmark1000: BenchmarkData = BenchmarkData.load_with_count(1000, path=path)
        assert benchmark1000.get_tuple_count() == 1000

    def test_can_get_largest_mean_from_group(self):
        benchmark_data: BenchmarkData = get_test_benchmark_data()
        group: str = "join on onePercent"
        expected: float = 0.002630475582435891
        compare_floats(
            benchmark_data.get_largest_mean_from_group(group), expected, 0.00001
        )
