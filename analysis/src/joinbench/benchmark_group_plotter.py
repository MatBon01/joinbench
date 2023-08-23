from typing import List

from .benchmark_group import BenchmarkGroup


class BenchmarkGroupPlotter:
    def __init__(self, benchmarks: BenchmarkGroup):
        self.benchmarks: BenchmarkGroup = benchmarks

    def plot_mean_time_of_group_by_tuple_count(self, ax, group: str) -> None:
        ax.set_title(f"Mean time to complete `{group}' query according to tuple count")
        ax.set_xlabel("Tuple count")
        ax.set_ylabel("Mean time (s)")
        xs: List[int] = self.benchmarks.get_tuple_counts()
        for function in self.benchmarks.get_function_name_list():
            ys = self.benchmarks.get_benchmark_means(group, function)
            ax.plot(xs, ys, "o-", label=function)
        ax.legend()
