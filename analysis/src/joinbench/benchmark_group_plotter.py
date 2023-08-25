from typing import List

from .benchmark_group import BenchmarkGroup


class BenchmarkGroupPlotter:
    def __init__(self, benchmarks: BenchmarkGroup):
        self.benchmarks: BenchmarkGroup = benchmarks

    def plot_mean_time_of_group_by_tuple_count(self, ax, group: str) -> None:
        ax.set_title(f"Mean time to complete `{group}' query\naccording to tuple count")
        ax.set_xlabel("Tuple count")
        ax.set_ylabel("Mean time (s)")
        self.bare_plot_mean_time_of_group_by_tuple_count(ax, group)
        ax.legend()

    def bare_plot_mean_time_of_group_by_tuple_count(self, ax, group: str) -> None:
        xs: List[int] = self.benchmarks.get_tuple_counts()
        for function in self.benchmarks.get_function_name_list():
            ys = self.benchmarks.get_benchmark_means(group, function)
            ax.plot(xs, ys, "o-", label=function)

    def plot_mean_time_of_group_by_tuple_count_with_inset_axes(
        self,
        ax,
        group: str,
        num_counts_in_inset: int,
        inset_dimensions: List[float],
        inset_scale_multiplier: int,
    ) -> None:
        self.plot_mean_time_of_group_by_tuple_count(ax, group)

        inset_counts: List[int] = sorted(self.benchmarks.get_tuple_counts())[
            :num_counts_in_inset
        ]
        inset_benchmarks: BenchmarkGroup = self.benchmarks.make_subgroup_with_counts(
            inset_counts
        )
        inset_plotter: BenchmarkGroupPlotter = BenchmarkGroupPlotter(inset_benchmarks)

        axins = ax.inset_axes(inset_dimensions)
        inset_plotter.bare_plot_mean_time_of_group_by_tuple_count(axins, group)

        axins.set_xlim(0, inset_counts[-1] * inset_scale_multiplier)
        axins.set_ylim(
            0,
            inset_benchmarks.get_largest_mean_from_experiment_group(group)
            * inset_scale_multiplier,
        )

        ax.indicate_inset_zoom(axins, edgecolor="black")

    def plot_mean_time_by_tuples_for_function(self, ax, function: str):
        xs: List[int] = self.benchmarks.get_tuple_counts()
        queries: List[str] = self.benchmarks.get_query_list()
        for query in queries:
            ys = self.benchmarks.get_benchmark_means(query, function)
            ax.plot(xs, ys, "o-", label=query)

        ax.legend()
        ax.set_title(f"Mean time for `{function.lower()}' function\nto complete queries by tuple count")
        ax.set_xlabel("Tuple count")
        ax.set_ylabel("Mean time (s)")
