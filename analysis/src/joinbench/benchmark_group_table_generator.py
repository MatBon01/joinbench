from typing import List, Tuple

import pandas as pd

from joinbench.benchmark_data import BenchmarkData
from joinbench.benchmark_group import BenchmarkGroup


class BenchmarkGroupTableGenerator:
    def __init__(self, benchmark_group: BenchmarkGroup):
        self.benchmark_group: BenchmarkGroup = benchmark_group

    @staticmethod
    def calculate_percentage_change(old_value: float, new_value: float) -> float:
        return (new_value - old_value) / old_value * 100

    def get_percentage_change_of_indexed_equijoin_for_counts(
        self, counts: List[int], query
    ) -> pd.DataFrame:
        percentage_change_of_means: pd.DataFrame = pd.DataFrame(
            index=["Product equijoin", "Comprehension equijoin"]
        )
        for benchmark in self.benchmark_group.get_list_of_benchmarks_with_counts(
            counts
        ):
            p_mean = benchmark.get_benchmark_mean(query, "Product equijoin")
            i_mean = benchmark.get_benchmark_mean(query, "Indexed equijoin")
            c_mean = benchmark.get_benchmark_mean(query, "Comprehension equijoin")

            p_percent_change = (i_mean - p_mean) / p_mean * 100
            c_percent_change = (i_mean - c_mean) / c_mean * 100
            percentage_change_of_means[f"{benchmark.get_tuple_count()} tuples"] = [
                p_percent_change,
                c_percent_change,
            ]

        return percentage_change_of_means

    def get_mean_and_std_dev_for_query_and_counts(
        self, query: str, counts: List[int]
    ) -> pd.DataFrame:
        functions: List[str] = self.benchmark_group.get_function_name_list()
        mean_with_sd: pd.DataFrame = pd.DataFrame(index=functions)
        count: int
        for count in counts:
            benchmark: BenchmarkData = self.benchmark_group.get_benchmark_with_count(
                count
            )
            means: List[float] = list(
                map(
                    lambda function: benchmark.get_benchmark_mean(query, function),
                    functions,
                )
            )
            std_devs: List[float] = list(
                map(
                    lambda function: benchmark.get_standard_deviation(query, function),
                    functions,
                )
            )
            combined: List[Tuple[float, float]] = list(zip(means, std_devs))
            formatted: List[str] = list(
                map(lambda pair: f"{pair[0]:.3g} ± {pair[1]:.3g}", combined)
            )
            mean_with_sd[f"{count} tuples"] = formatted
        return mean_with_sd
