from typing import List, Set

from joinbench.benchmark import Benchmark


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


def get_test_data_report_names() -> Set[str]:
    return {
        "join on onePercent/modular product",
        "join on onePercent/modular indexed",
        "join on onePercent/old comprehension",
        "join onePercent and twentyPercent/modular product",
        "join onePercent and twentyPercent/modular indexed",
        "join onePercent and twentyPercent/old comprehension",
        "join onePercent and fiftyPercent/modular product",
        "join onePercent and fiftyPercent/modular indexed",
        "join onePercent and fiftyPercent/old comprehension",
        "join even and odd/modular product",
        "join even and odd/modular indexed",
        "join even and odd/old comprehension",
    }
