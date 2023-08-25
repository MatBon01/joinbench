from typing import Dict, List, Tuple

import numpy as np

from .benchmark_data import BenchmarkData


class BenchmarkPlotter:
    def __init__(self, data: BenchmarkData):
        self.data = data

    def plot_benchmark_group_means(self, ax, group_name: str):
        group_map: Dict[
            str, List[Tuple[int, str]]
        ] = self.data.map_benchmark_groups_and_benchmark_indices()
        experiments: List[Tuple[int, str]] = group_map[group_name]

        indices: List[int]
        names: List[str]
        indices, names = zip(*experiments)
        means = self.data.get_means_of_benchmark_list(indices)

        ax.set_title(
            f"Mean time to complete `{group_name}' with {self.data.get_tuple_count()} tuples"
        )
        ax.set_ylabel("Time (s)")
        ax.set_xlabel("Function")

        ax.bar(names, means)

        return ax

    def plot_given_queries(self, ax, queries_to_display: List[str]):
        # NOTE: queries_to_display may have \n in the string for
        # display purposes
        functions = self.data.get_function_name_list()
        queries = list(
            map(lambda query: "".join(query.split("\n")), queries_to_display)
        )

        function_means = {
            function: self.data.get_means_of_function_for_queries(function, queries)
            for function in functions
        }

        x = np.arange(len(queries))  # the label locations
        width = 0.25  # the width of the bars
        multiplier = 0

        for function, mean in function_means.items():
            offset = width * multiplier
            ax.bar(x + offset, mean, width, label=function)
            multiplier += 1

        ax.set_ylabel("Mean time (s)")
        ax.set_xlabel("Query")
        ax.set_title(
            f"Mean time to complete given queries with {self.data.get_tuple_count()} tuples"
        )
        ax.set_xticks(x + width, queries_to_display)
        ax.legend(loc="upper left")

    def compare_query_means(self, ax, queries: str) -> None:
        functions = self.data.get_function_name_list()

        query_means = {
            query: self.data.get_means_of_query_for_functions(query, functions)
            for query in queries
        }

        x = np.arange(len(functions))  # the label locations
        width = 0.25 * (3 / len(queries))# the width of the bars
        multiplier = 0

        for query, mean in query_means.items():
            offset = width * multiplier
            ax.bar(x + offset, mean, width, label=query)
            multiplier += 1

        ax.set_ylabel("Mean time (s)")
        ax.set_xlabel("Function")
        ax.set_title(
            f"Mean time to complete given queries with {self.data.get_tuple_count()} tuples"
        )
        ax.set_xticks(x + ((width * (len(queries) - 1) / 2)), functions)
        ax.legend(loc="upper left")
