from typing import Dict, List, Set, Tuple

from joinbench.benchmark import Benchmark
from joinbench.benchmark_data import BenchmarkData


def get_test_benchmarks() -> List[Benchmark]:
    b1 = Benchmark()
    b2 = Benchmark()
    b3 = Benchmark()
    b4 = Benchmark()
    b5 = Benchmark()

    bs = [b1, b2, b3, b4, b5]
    return bs


def get_test_data_location() -> str:
    return "test/joinbench/test_data.json"


def get_test_benchmark_data() -> BenchmarkData:
    return BenchmarkData(get_test_data_location())


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
