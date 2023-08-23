from typing import Dict, List, Set, Tuple

from joinbench.benchmark_data import BenchmarkData
from joinbench.benchmark_group import BenchmarkGroup


def get_test_data_location() -> str:
    return f"{get_joinbench_test_data_directory_path()}test_data.json"


def get_joinbench_test_data_directory_path() -> str:
    return "test/joinbench/test_data/"


def get_joinbench_benchmarks() -> List[BenchmarkData]:
    path: str = get_joinbench_test_data_directory_path()
    return [
        BenchmarkData(f"{path}joinbench127.json", 127),
        BenchmarkData(f"{path}joinbench1000.json", 1000),
    ]


def get_joinbench_benchmark_group() -> BenchmarkGroup:
    benchmarks: List[BenchmarkData] = get_joinbench_benchmarks()
    return BenchmarkGroup(benchmarks)


def get_test_benchmark_data() -> BenchmarkData:
    return BenchmarkData(
        get_test_data_location(), get_test_benchmark_data_number_of_tuples()
    )


def get_test_benchmark_data_number_of_tuples() -> int:
    return 100


def get_list_of_data_report_names() -> List[str]:
    return [
        "join on onePercent/modular product",
        "join on onePercent/old comprehension",
        "join on onePercent/modular indexed",
        "join onePercent and twentyPercent/modular product",
        "join onePercent and twentyPercent/old comprehension",
        "join onePercent and twentyPercent/modular indexed",
        "join onePercent and fiftyPercent/modular product",
        "join onePercent and fiftyPercent/old comprehension",
        "join onePercent and fiftyPercent/modular indexed",
        "join even and odd/modular product",
        "join even and odd/old comprehension",
        "join even and odd/modular indexed",
    ]


def get_test_data_report_names() -> Set[str]:
    return set(get_list_of_data_report_names())


def get_test_data_benchmark_group_names() -> Set[str]:
    return {
        "join on onePercent",
        "join onePercent and twentyPercent",
        "join onePercent and fiftyPercent",
        "join even and odd",
    }


def get_test_data_benchmark_group_and_benchmark_mapping() -> (
    Dict[str, List[Tuple[int, str]]]
):
    return {
        "join on onePercent": [
            (0, "modular product"),
            (1, "old comprehension"),
            (2, "modular indexed"),
        ],
        "join onePercent and twentyPercent": [
            (3, "modular product"),
            (4, "old comprehension"),
            (5, "modular indexed"),
        ],
        "join onePercent and fiftyPercent": [
            (6, "modular product"),
            (7, "old comprehension"),
            (8, "modular indexed"),
        ],
        "join even and odd": [
            (9, "modular product"),
            (10, "old comprehension"),
            (11, "modular indexed"),
        ],
    }


def get_function_names() -> List[str]:
    return ["modular product", "old comprehension", "modular indexed"]

def compare_floats(f1: float, f2: float, error_scale: float):
    assert f2 * (1 - error_scale) < f1 and f1 < f2 * (1 + error_scale)
