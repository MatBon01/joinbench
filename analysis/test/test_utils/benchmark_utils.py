from typing import List

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
