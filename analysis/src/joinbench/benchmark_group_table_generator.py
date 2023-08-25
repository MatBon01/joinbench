from typing import List

import pandas as pd

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
